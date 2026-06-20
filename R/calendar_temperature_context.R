############################################################
# Calendar Temperature Context Helpers
# File: R/calendar_temperature_context.R
############################################################

library(dplyr)
library(httr2)
library(lubridate)
library(tibble)

archive_daily_variable_for_temperature <- function(kind, measure = NULL) {
  if (identical(kind, "air")) {
    return("temperature_2m_mean")
  }

  measure <- ifelse(is.null(measure), "", measure)

  switch(
    measure,
    soil_15cm = "soil_temperature_7_to_28cm_mean",
    soil_temperature_18cm = "soil_temperature_7_to_28cm_mean",
    soil_temperature_54cm = "soil_temperature_28_to_100cm_mean",
    "soil_temperature_0_to_7cm_mean"
  )
}

temperature_context_label <- function(kind, measure = NULL) {
  if (identical(kind, "air")) {
    return("Air temperature at 2 m")
  }

  measure <- ifelse(is.null(measure), "", measure)

  switch(
    measure,
    soil_15cm = "Soil temperature, 7-28 cm reference",
    soil_temperature_18cm = "Soil temperature, 7-28 cm reference",
    soil_temperature_54cm = "Soil temperature, 28-100 cm reference",
    "Soil temperature, 0-7 cm reference"
  )
}

fetch_openmeteo_archive_daily_temperature <- function(lat,
                                                      lon,
                                                      start_date,
                                                      end_date,
                                                      daily_var) {
  req <- request("https://archive-api.open-meteo.com/v1/archive") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = as.character(as.Date(start_date)),
      end_date = as.character(as.Date(end_date)),
      daily = daily_var,
      timezone = "auto",
      models = "era5_land"
    )

  js <- perform_json_request(req, timeout_seconds = 30, max_retries = 2)

  if (is.null(js$daily) || is.null(js$daily$time) || is.null(js$daily[[daily_var]])) {
    stop("Open-Meteo archive response is missing daily temperature data.")
  }

  tibble(
    date = as.Date(js$daily$time),
    mean_temp_c = as.numeric(js$daily[[daily_var]]),
    provider = "Open-Meteo ERA5-Land"
  ) %>%
    filter(!is.na(date), !is.na(mean_temp_c))
}

build_daily_temperature <- function(df_hourly, start_date) {
  required_cols <- c("datetime", "temp_c", "source")
  missing_cols <- setdiff(required_cols, names(df_hourly))

  if (length(missing_cols) > 0) {
    stop(
      "build_daily_temperature() is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!("provider" %in% names(df_hourly))) {
    df_hourly$provider <- "Weather API"
  }

  df_hourly %>%
    mutate(
      datetime = as.POSIXct(datetime),
      date = as.Date(datetime)
    ) %>%
    filter(date >= as.Date(start_date), !is.na(temp_c)) %>%
    group_by(date, source, provider) %>%
    summarise(mean_temp_c = mean(temp_c, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      source_order = match(source, c("Observed", "Provisional", "Forecast"))
    ) %>%
    arrange(date, source_order) %>%
    distinct(date, .keep_all = TRUE) %>%
    select(date, source, provider, mean_temp_c)
}

build_temperature_normals <- function(lat,
                                      lon,
                                      daily_var,
                                      start_year = 1991,
                                      end_year = 2020,
                                      plot_year = as.integer(format(Sys.Date(), "%Y"))) {
  archive <- fetch_openmeteo_archive_daily_temperature(
    lat = lat,
    lon = lon,
    start_date = as.Date(paste0(start_year, "-01-01")),
    end_date = as.Date(paste0(end_year, "-12-31")),
    daily_var = daily_var
  )

  archive %>%
    mutate(month_day = format(date, "%m-%d")) %>%
    filter(month_day != "02-29") %>%
    group_by(month_day) %>%
    summarise(
      long_term_mean_c = mean(mean_temp_c, na.rm = TRUE),
      sd_temp_c = sd(mean_temp_c, na.rm = TRUE),
      n_years = sum(!is.na(mean_temp_c)),
      .groups = "drop"
    ) %>%
    mutate(
      date = as.Date(paste0(plot_year, "-", month_day)),
      sd_temp_c = ifelse(is.na(sd_temp_c), 0, sd_temp_c),
      lo_1sd = long_term_mean_c - sd_temp_c,
      hi_1sd = long_term_mean_c + sd_temp_c,
      lo_2sd = long_term_mean_c - 2 * sd_temp_c,
      hi_2sd = long_term_mean_c + 2 * sd_temp_c
    ) %>%
    arrange(date)
}
