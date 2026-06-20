import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Local Java iteration of the Shiny degree-day pipeline:
 * hourly weather -> daily mean/DD -> cumulative DD -> threshold crossings.
 *
 * Input CSV format (required header):
 * datetime,temp_c,source
 * 2026-02-01 00:00:00,4.2,Observed
 * 2026-02-01 01:00:00,4.1,Observed
 * ...
 */
public final class LocalDegreeDayIteration {
    private static final DateTimeFormatter OUTPUT_DATE = DateTimeFormatter.ISO_LOCAL_DATE;

    private LocalDegreeDayIteration() {
    }

    public static void main(String[] args) throws IOException {
        Args parsed = Args.parse(args);

        List<HourlyRow> hourly = readHourlyCsv(parsed.inputCsv());
        List<DailySourceRow> dailyBySource = buildDailyDegreeDays(hourly, parsed.plantingDate(), parsed.baseTemp());
        List<DailyRow> cumulative = buildDegreeDays(dailyBySource, parsed.todayDate());
        List<ThresholdCrossing> crossings = findThresholdCrossings(cumulative, parsed.thresholds());

        printSummary(parsed, cumulative, crossings);

        if (parsed.outputCsv() != null) {
            writeDailyCsv(cumulative, parsed.outputCsv());
            System.out.println("Wrote: " + parsed.outputCsv());
        }
    }

    private static void printSummary(Args parsed, List<DailyRow> cumulative, List<ThresholdCrossing> crossings) {
        System.out.println("=== Local Degree Day Iteration ===");
        System.out.println("Input CSV: " + parsed.inputCsv());
        System.out.println("Planting date: " + parsed.plantingDate());
        System.out.println("Base temp (C): " + parsed.baseTemp());
        System.out.println("Today date: " + parsed.todayDate());
        System.out.println("Daily rows: " + cumulative.size());

        if (!cumulative.isEmpty()) {
            DailyRow last = cumulative.get(cumulative.size() - 1);
            System.out.printf(Locale.US, "Last cumulative DD: %.2f (%s)%n", last.cumDd(), last.date());
        }

        System.out.println();
        System.out.println("Threshold crossings:");
        for (ThresholdCrossing crossing : crossings) {
            if (crossing.date() == null) {
                System.out.printf(Locale.US, "- %.1f: not reached%n", crossing.threshold());
            } else {
                System.out.printf(
                    Locale.US,
                    "- %.1f: %s (%s, cum_dd=%.2f)%n",
                    crossing.threshold(),
                    crossing.date(),
                    crossing.source(),
                    crossing.cumDd()
                );
            }
        }
    }

    private static List<HourlyRow> readHourlyCsv(Path csvPath) throws IOException {
        if (!Files.exists(csvPath)) {
            throw new IllegalArgumentException("Input file does not exist: " + csvPath);
        }

        List<HourlyRow> rows = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(csvPath, StandardCharsets.UTF_8)) {
            String header = reader.readLine();
            if (header == null) {
                throw new IllegalArgumentException("CSV is empty: " + csvPath);
            }

            String[] cols = splitCsvLine(header);
            int idxDatetime = findColumn(cols, "datetime");
            int idxTemp = findColumn(cols, "temp_c");
            int idxSource = findColumn(cols, "source");
            if (idxDatetime < 0 || idxTemp < 0 || idxSource < 0) {
                throw new IllegalArgumentException("CSV must include headers: datetime,temp_c,source");
            }

            String line;
            int lineNumber = 1;
            while ((line = reader.readLine()) != null) {
                lineNumber++;
                if (line.trim().isEmpty()) {
                    continue;
                }
                String[] parts = splitCsvLine(line);
                if (parts.length <= Math.max(idxDatetime, Math.max(idxTemp, idxSource))) {
                    throw new IllegalArgumentException("Malformed CSV row at line " + lineNumber + ": " + line);
                }

                String dtRaw = parts[idxDatetime].trim();
                String tempRaw = parts[idxTemp].trim();
                String sourceRaw = parts[idxSource].trim();

                LocalDateTime dt = parseDateTime(dtRaw);
                double temp = Double.parseDouble(tempRaw);
                Source source = Source.parse(sourceRaw);

                rows.add(new HourlyRow(dt, temp, source));
            }
        }

        rows.sort(Comparator.comparing(HourlyRow::datetime));
        return rows;
    }

    private static int findColumn(String[] headers, String target) {
        for (int i = 0; i < headers.length; i++) {
            if (headers[i].trim().equalsIgnoreCase(target)) {
                return i;
            }
        }
        return -1;
    }

    private static String[] splitCsvLine(String line) {
        List<String> out = new ArrayList<>();
        StringBuilder cur = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                inQuotes = !inQuotes;
                continue;
            }
            if (ch == ',' && !inQuotes) {
                out.add(cur.toString());
                cur.setLength(0);
            } else {
                cur.append(ch);
            }
        }
        out.add(cur.toString());
        return out.toArray(new String[0]);
    }

    private static LocalDateTime parseDateTime(String input) {
        String normalized = input.trim().replace('T', ' ');
        List<DateTimeFormatter> formats = List.of(
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"),
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"),
            DateTimeFormatter.ofPattern("yyyy-MM-dd H:mm:ss"),
            DateTimeFormatter.ofPattern("yyyy-MM-dd H:mm")
        );

        for (DateTimeFormatter f : formats) {
            try {
                return LocalDateTime.parse(normalized, f);
            } catch (DateTimeParseException ignored) {
            }
        }

        try {
            return LocalDateTime.parse(input);
        } catch (DateTimeParseException ex) {
            throw new IllegalArgumentException("Cannot parse datetime: " + input);
        }
    }

    private static List<DailySourceRow> buildDailyDegreeDays(
        List<HourlyRow> hourlyRows,
        LocalDate plantingDate,
        double baseTemp
    ) {
        Map<DateSourceKey, RunningStats> grouped = new HashMap<>();

        for (HourlyRow row : hourlyRows) {
            LocalDate date = row.datetime().toLocalDate();
            if (date.isBefore(plantingDate)) {
                continue;
            }

            DateSourceKey key = new DateSourceKey(date, row.source());
            grouped.computeIfAbsent(key, k -> new RunningStats()).add(row.tempC());
        }

        List<DailySourceRow> out = new ArrayList<>(grouped.size());
        for (Map.Entry<DateSourceKey, RunningStats> entry : grouped.entrySet()) {
            DateSourceKey key = entry.getKey();
            double meanTemp = entry.getValue().mean();
            double ddDay = calcDailyDd(meanTemp, baseTemp);
            out.add(new DailySourceRow(key.date(), key.source(), meanTemp, ddDay));
        }

        out.sort(Comparator.comparing(DailySourceRow::date).thenComparing(DailySourceRow::source));
        return out;
    }

    private static double calcDailyDd(double meanTempC, double baseTemp) {
        return Math.max(0.0, meanTempC - baseTemp);
    }

    private static List<DailyRow> buildDegreeDays(List<DailySourceRow> dailyBySource, LocalDate todayDate) {
        Map<LocalDate, RunningDailyStats> observedByDate = new LinkedHashMap<>();
        Map<LocalDate, RunningDailyStats> forecastByDate = new LinkedHashMap<>();

        for (DailySourceRow row : dailyBySource) {
            Map<LocalDate, RunningDailyStats> target = !row.date().isAfter(todayDate) ? observedByDate : forecastByDate;
            target.computeIfAbsent(row.date(), d -> new RunningDailyStats()).add(row.meanTempC(), row.ddDay());
        }

        List<DailyRow> observed = new ArrayList<>();
        List<LocalDate> observedDates = new ArrayList<>(observedByDate.keySet());
        observedDates.sort(Comparator.naturalOrder());

        double cum = 0.0;
        for (LocalDate date : observedDates) {
            RunningDailyStats stats = observedByDate.get(date);
            double meanTemp = stats.meanTempMean();
            double ddDay = stats.ddDayMean();
            cum += ddDay;
            observed.add(new DailyRow(date, Source.OBSERVED, meanTemp, ddDay, cum));
        }

        List<DailyRow> forecast = new ArrayList<>();
        List<LocalDate> forecastDates = new ArrayList<>(forecastByDate.keySet());
        forecastDates.sort(Comparator.naturalOrder());
        for (LocalDate date : forecastDates) {
            RunningDailyStats stats = forecastByDate.get(date);
            double meanTemp = stats.meanTempMean();
            double ddDay = stats.ddDayMean();
            cum += ddDay;
            forecast.add(new DailyRow(date, Source.FORECAST, meanTemp, ddDay, cum));
        }

        List<DailyRow> all = new ArrayList<>(observed.size() + forecast.size());
        all.addAll(observed);
        all.addAll(forecast);
        all.sort(Comparator.comparing(DailyRow::date));
        return all;
    }

    private static List<ThresholdCrossing> findThresholdCrossings(List<DailyRow> dailyRows, List<Double> thresholds) {
        List<ThresholdCrossing> out = new ArrayList<>(thresholds.size());
        List<DailyRow> sorted = new ArrayList<>(dailyRows);
        sorted.sort(Comparator.comparing(DailyRow::date));

        for (double threshold : thresholds) {
            ThresholdCrossing hit = null;
            for (DailyRow row : sorted) {
                if (row.cumDd() >= threshold) {
                    hit = new ThresholdCrossing(threshold, row.date(), row.source(), row.cumDd());
                    break;
                }
            }
            if (hit == null) {
                hit = new ThresholdCrossing(threshold, null, null, Double.NaN);
            }
            out.add(hit);
        }

        return out;
    }

    private static void writeDailyCsv(List<DailyRow> rows, Path outputCsv) throws IOException {
        Path parent = outputCsv.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }

        try (BufferedWriter writer = Files.newBufferedWriter(outputCsv, StandardCharsets.UTF_8)) {
            writer.write("date,source,mean_temp_c,dd_day,cum_dd");
            writer.newLine();
            for (DailyRow row : rows) {
                writer.write(
                    row.date().format(OUTPUT_DATE) + ","
                        + row.source().label + ","
                        + formatDouble(row.meanTempC()) + ","
                        + formatDouble(row.ddDay()) + ","
                        + formatDouble(row.cumDd())
                );
                writer.newLine();
            }
        }
    }

    private static String formatDouble(double v) {
        return String.format(Locale.US, "%.4f", v);
    }

    private record HourlyRow(LocalDateTime datetime, double tempC, Source source) {
    }

    private record DailySourceRow(LocalDate date, Source source, double meanTempC, double ddDay) {
    }

    private record DailyRow(LocalDate date, Source source, double meanTempC, double ddDay, double cumDd) {
    }

    private record ThresholdCrossing(double threshold, LocalDate date, Source source, double cumDd) {
    }

    private record DateSourceKey(LocalDate date, Source source) {
    }

    private enum Source {
        OBSERVED("Observed"),
        FORECAST("Forecast");

        private final String label;

        Source(String label) {
            this.label = label;
        }

        static Source parse(String raw) {
            String norm = raw == null ? "" : raw.trim().toLowerCase(Locale.US);
            if (norm.equals("observed")) {
                return OBSERVED;
            }
            if (norm.equals("forecast")) {
                return FORECAST;
            }
            throw new IllegalArgumentException("Unknown source value: " + raw + " (expected Observed or Forecast)");
        }
    }

    private static final class RunningStats {
        private double sum;
        private long count;

        void add(double value) {
            if (!Double.isNaN(value)) {
                sum += value;
                count++;
            }
        }

        double mean() {
            if (count == 0) {
                return Double.NaN;
            }
            return sum / count;
        }
    }

    private static final class RunningDailyStats {
        private final RunningStats meanTemps = new RunningStats();
        private final RunningStats ddDays = new RunningStats();

        void add(double meanTemp, double ddDay) {
            meanTemps.add(meanTemp);
            ddDays.add(ddDay);
        }

        double meanTempMean() {
            return meanTemps.mean();
        }

        double ddDayMean() {
            return ddDays.mean();
        }
    }

    private record Args(
        Path inputCsv,
        LocalDate plantingDate,
        double baseTemp,
        LocalDate todayDate,
        List<Double> thresholds,
        Path outputCsv
    ) {
        static Args parse(String[] args) {
            Map<String, String> kv = parseArgsToMap(args);

            String inputRaw = require(kv, "input");
            String plantingRaw = require(kv, "planting-date");

            Path inputCsv = Path.of(inputRaw);
            LocalDate planting = LocalDate.parse(plantingRaw);
            double baseTemp = parseDouble(kv.getOrDefault("base-temp", "5"));
            LocalDate today = LocalDate.parse(kv.getOrDefault("today", LocalDate.now().toString()));
            double risk1 = parseDouble(kv.getOrDefault("risk1", "400"));
            double risk2 = parseDouble(kv.getOrDefault("risk2", "600"));
            double risk3 = parseDouble(kv.getOrDefault("risk3", "800"));

            if (!(risk1 < risk2 && risk2 < risk3)) {
                throw new IllegalArgumentException("Risk thresholds must satisfy risk1 < risk2 < risk3");
            }

            Path output = kv.containsKey("output") ? Path.of(kv.get("output")) : null;

            return new Args(inputCsv, planting, baseTemp, today, List.of(risk1, risk2, risk3), output);
        }

        private static Map<String, String> parseArgsToMap(String[] args) {
            Map<String, String> out = new HashMap<>();
            for (int i = 0; i < args.length; i++) {
                String arg = args[i];
                if (!arg.startsWith("--")) {
                    usageAndFail("Unexpected argument: " + arg);
                }
                String key = arg.substring(2);
                if (key.equals("help")) {
                    usageAndFail(null);
                }
                if (i + 1 >= args.length || args[i + 1].startsWith("--")) {
                    usageAndFail("Missing value for --" + key);
                }
                String value = args[++i];
                out.put(key, value);
            }
            return out;
        }

        private static String require(Map<String, String> kv, String key) {
            if (!kv.containsKey(key)) {
                usageAndFail("Missing required argument: --" + key);
            }
            return Objects.requireNonNull(kv.get(key));
        }

        private static double parseDouble(String raw) {
            try {
                return Double.parseDouble(raw);
            } catch (NumberFormatException ex) {
                throw new IllegalArgumentException("Invalid number: " + raw);
            }
        }

        private static void usageAndFail(String error) {
            if (error != null) {
                System.err.println("Error: " + error);
                System.err.println();
            }
            System.err.println("Usage:");
            System.err.println("  java LocalDegreeDayIteration \\");
            System.err.println("    --input <hourly.csv> \\");
            System.err.println("    --planting-date <YYYY-MM-DD> \\");
            System.err.println("    [--base-temp <double>] \\");
            System.err.println("    [--today <YYYY-MM-DD>] \\");
            System.err.println("    [--risk1 <double>] [--risk2 <double>] [--risk3 <double>] \\");
            System.err.println("    [--output <daily_output.csv>]");
            System.err.println();
            System.err.println("Datetime examples accepted:");
            System.err.println("  2026-03-01 14:00:00");
            System.err.println("  2026-03-01T14:00:00");
            throw new IllegalArgumentException("Invalid arguments");
        }
    }
}
