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
#
# Expected source values:
#   - "Observed"
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
  
  df_hourly %>%
    mutate(
      datetime = as.POSIXct(datetime),
      date = as.Date(datetime)
    ) %>%
    filter(date >= planting_date) %>%
    group_by(date, source) %>%
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
#   2. Collapse observed dates into one daily record
#   3. Collapse forecast dates into one daily record
#   4. Compute cumulative DD
#   5. Forecast cumulative DD starts from last observed value
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
#     - mean_temp_c
#     - dd_day
#     - cum_dd
############################################################

build_degree_days <- function(df_hourly, planting_date, base_temp = 5) {
  
  planting_date <- as.Date(planting_date)
  today_local <- Sys.Date()
  
  df_daily <- build_daily_degree_days(
    df_hourly = df_hourly,
    planting_date = planting_date,
    base_temp = base_temp
  )
  
  ##########################################################
  # Observed portion
  ##########################################################
  observed <- df_daily %>%
    filter(date <= today_local) %>%
    group_by(date) %>%
    summarise(
      source = "Observed",
      mean_temp_c = mean(mean_temp_c, na.rm = TRUE),
      dd_day = mean(dd_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  if (nrow(observed) > 0) {
    observed <- observed %>%
      mutate(cum_dd = cumsum(dd_day))
  } else {
    observed <- tibble(
      date = as.Date(character()),
      source = character(),
      mean_temp_c = numeric(),
      dd_day = numeric(),
      cum_dd = numeric()
    )
  }
  
  ##########################################################
  # Forecast portion
  ##########################################################
  forecast <- df_daily %>%
    filter(date > today_local) %>%
    group_by(date) %>%
    summarise(
      source = "Forecast",
      mean_temp_c = mean(mean_temp_c, na.rm = TRUE),
      dd_day = mean(dd_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  last_obs_cum_dd <- if (nrow(observed) > 0) {
    tail(observed$cum_dd, 1)
  } else {
    0
  }
  
  if (nrow(forecast) > 0) {
    forecast <- forecast %>%
      mutate(cum_dd = last_obs_cum_dd + cumsum(dd_day))
  } else {
    forecast <- tibble(
      date = as.Date(character()),
      source = character(),
      mean_temp_c = numeric(),
      dd_day = numeric(),
      cum_dd = numeric()
    )
  }
  
  ##########################################################
  # Return combined daily table
  ##########################################################
  bind_rows(observed, forecast) %>%
    arrange(date)
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