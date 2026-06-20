############################################################
# Degree Day Functions
# File: R/degree_days.R
#
# Purpose:
#   Convert hourly soil temperature data into:
#     - daily mean temperature
#     - daily degree days above a base temperature
#     - cumulative degree days from planting date onward
#
# Expected input columns in df_hourly:
#   - datetime
#   - temp_c
#   - source
#   - provider (optional)
#
# Expected source values:
#   - "Observed"
#   - "Provisional"
#   - "Forecast"
############################################################

library(dplyr)
library(lubridate)
library(tibble)

############################################################
# Calculate daily degree days from daily mean temperature
#
# Args:
#   mean_temp_c : numeric vector of daily mean temperatures
#   base_temp   : numeric scalar base temperature
#
# Returns:
#   numeric vector of daily degree days
############################################################

calc_daily_dd <- function(mean_temp_c, base_temp = 5) {
  pmax(0, mean_temp_c - base_temp)
}

############################################################
# Aggregate hourly temperature data to daily values
#
# Args:
#   df_hourly   : data frame with datetime, temp_c, source
#   planting_date : Date or date-like string
#   base_temp   : numeric scalar
#
# Returns:
#   data frame with:
#     - date
#     - source
#     - provider
#     - mean_temp_c
#     - dd_day
############################################################

build_daily_degree_days <- function(df_hourly, planting_date, base_temp = 5) {
  
  planting_date <- as.Date(planting_date)
  
  required_cols <- c("datetime", "temp_c", "source")
  missing_cols <- setdiff(required_cols, names(df_hourly))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "build_daily_degree_days() is missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
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
    filter(date >= planting_date, !is.na(temp_c)) %>%
    group_by(date, source, provider) %>%
    summarise(
      mean_temp_c = mean(temp_c, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dd_day = calc_daily_dd(mean_temp_c, base_temp = base_temp)
    ) %>%
    arrange(date)
}

############################################################
# Build cumulative degree days
#
# Logic:
#   1. Build daily DD by source
#   2. Keep source labels visible: Observed, Provisional, Forecast
#   3. Compute one continuous cumulative DD series in date order
#
# Args:
#   df_hourly      : data frame with datetime, temp_c, source
#   planting_date  : Date or date-like string
#   base_temp      : numeric scalar
#
# Returns:
#   data frame with:
#     - date
#     - source
#     - provider
#     - mean_temp_c
#     - dd_day
#     - cum_dd
############################################################

build_degree_days <- function(df_hourly, planting_date, base_temp = 5) {
  
  planting_date <- as.Date(planting_date)
  df_daily <- build_daily_degree_days(
    df_hourly = df_hourly,
    planting_date = planting_date,
    base_temp = base_temp
  )

  if (nrow(df_daily) == 0) {
    return(tibble(
      date = as.Date(character()),
      source = character(),
      provider = character(),
      mean_temp_c = numeric(),
      dd_day = numeric(),
      cum_dd = numeric()
    ))
  }

  source_levels <- c("Observed", "Provisional", "Forecast")

  df_daily %>%
    mutate(
      source = ifelse(source %in% source_levels, source, "Observed"),
      source_order = match(source, source_levels)
    ) %>%
    group_by(date, source, provider, source_order) %>%
    summarise(
      mean_temp_c = mean(mean_temp_c, na.rm = TRUE),
      dd_day = mean(dd_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date, source_order) %>%
    distinct(date, .keep_all = TRUE) %>%
    mutate(cum_dd = cumsum(dd_day)) %>%
    select(date, source, provider, mean_temp_c, dd_day, cum_dd)
}

############################################################
# Optional helper:
# Summarize where the cumulative DD crosses thresholds
#
# Args:
#   df_dd : output of build_degree_days()
#   thresholds : numeric vector
#
# Returns:
#   tibble with threshold and first crossing date
############################################################

find_threshold_crossings <- function(df_dd, thresholds) {
  
  if (!all(c("date", "cum_dd") %in% names(df_dd))) {
    stop("find_threshold_crossings() requires columns: date, cum_dd")
  }
  
  tibble(threshold = thresholds) %>%
    rowwise() %>%
    mutate(
      first_date = {
        idx <- which(df_dd$cum_dd >= threshold)
        if (length(idx) == 0) {
          NA_character_
        } else {
          as.character(df_dd$date[min(idx)])
        }
      }
    ) %>%
    ungroup()
}
