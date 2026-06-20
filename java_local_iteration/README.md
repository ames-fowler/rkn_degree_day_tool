# Local Java Iteration

This is a local Java CLI port of the Shiny degree-day core logic from:

- `R/degree_days.R` (`calc_daily_dd`, `build_daily_degree_days`, `build_degree_days`, threshold crossing behavior)

## What it does

Given hourly soil temperature data (`datetime,temp_c,source`), it:

1. Aggregates hourly rows to daily means by `date + source`
2. Computes daily degree days: `max(0, mean_temp_c - base_temp)`
3. Splits observed vs forecast at `today`
4. Builds cumulative DD where forecast accumulation continues from the last observed cumulative value
5. Reports first crossing date for each risk threshold

## Input CSV

Required header:

```csv
datetime,temp_c,source
2026-02-20 00:00:00,4.6,Observed
2026-02-20 01:00:00,4.2,Observed
2026-03-07 00:00:00,6.8,Forecast
```

- `source` must be `Observed` or `Forecast`
- datetime supports both `YYYY-MM-DD HH:mm:ss` and `YYYY-MM-DDTHH:mm:ss`

## Compile

```powershell
cd c:\projects\git\rkn_degree_day_tool\java_local_iteration
javac LocalDegreeDayIteration.java
```

## Run

```powershell
java LocalDegreeDayIteration `
  --input ..\data\hourly_soil_temp.csv `
  --planting-date 2026-02-15 `
  --base-temp 5 `
  --today 2026-03-06 `
  --risk1 400 --risk2 600 --risk3 800 `
  --output .\daily_degree_days.csv
```

Required args:

- `--input`
- `--planting-date`

Optional args:

- `--base-temp` (default `5`)
- `--today` (default system date)
- `--risk1`, `--risk2`, `--risk3` (defaults `400`, `600`, `800`)
- `--output` (if omitted, only console summary is printed)
