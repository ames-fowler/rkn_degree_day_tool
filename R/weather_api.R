############################################################
# Weather API Functions
# File: R/weather_api.R
#
# Purpose:
#   Pull hourly soil temperature from Open-Meteo for:
#     - historical / archived dates
#     - forecast dates
#
# Expected output columns:
#   - datetime
#   - temp_c
#   - source
#
# Notes:
#   - Uses timezone = "auto" so returned timestamps are local
#   - Designed to feed directly into build_degree_days()
############################################################

library(httr2)
library(tibble)
library(lubridate)
library(dplyr)

############################################################
# Validate requested soil temperature variable
############################################################

validate_soil_depth_var <- function(depth_var) {
  allowed <- c(
    "soil_temperature_0cm",
    "soil_temperature_6cm",
    "soil_temperature_18cm",
    "soil_temperature_54cm"
  )
  
  if (is.null(depth_var) || !(depth_var %in% allowed)) {
    stop(
      paste0(
        "depth_var must be one of: ",
        paste(allowed, collapse = ", ")
      )
    )
  }
  
  depth_var
}

############################################################
# Internal helper to convert Open-Meteo hourly payload
# into the standard app data frame
############################################################

parse_openmeteo_hourly <- function(js, depth_var, source_label) {
  if (is.null(js$hourly)) {
    stop("Open-Meteo response is missing 'hourly' data.")
  }
  
  if (is.null(js$hourly$time)) {
    stop("Open-Meteo response is missing hourly time values.")
  }
  
  vals <- js$hourly[[depth_var]]
  
  if (is.null(vals)) {
    stop(
      paste0(
        "Open-Meteo response is missing requested variable: ",
        depth_var
      )
    )
  }
  
  tz_out <- if (!is.null(js$timezone)) js$timezone else "UTC"
  
  time_chr <- gsub("T", " ", js$hourly$time, fixed = TRUE)
  
  tibble(
    datetime = lubridate::ymd_hms(
      time_chr,
      tz = tz_out,
      quiet = TRUE,
      truncated = 1
    ),
    temp_c = as.numeric(vals),
    source = source_label
  )
}
############################################################
# Fetch archived / historical hourly soil temperature
#
# Args:
#   lat        : numeric latitude
#   lon        : numeric longitude
#   start_date : "YYYY-MM-DD"
#   end_date   : "YYYY-MM-DD"
#   depth_var  : one of the supported soil temp variables
#
# Returns:
#   tibble(datetime, temp_c, source = "Observed")
############################################################

fetch_openmeteo_history <- function(lat,
                                    lon,
                                    start_date,
                                    end_date,
                                    depth_var = "soil_temperature_6cm") {
  
  depth_var <- validate_soil_depth_var(depth_var)
  
  start_date <- as.character(as.Date(start_date))
  end_date   <- as.character(as.Date(end_date))
  
  if (is.na(as.Date(start_date)) || is.na(as.Date(end_date))) {
    stop("start_date and end_date must be valid dates.")
  }
  
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("start_date cannot be after end_date.")
  }
  
  req <- request("https://historical-forecast-api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = start_date,
      end_date = end_date,
      hourly = depth_var,
      timezone = "auto"
    )

  js <- perform_json_request(req)
  
  parse_openmeteo_hourly(
    js = js,
    depth_var = depth_var,
    source_label = "Observed"
  )
}

############################################################
# Fetch forecast hourly soil temperature
#
# Args:
#   lat           : numeric latitude
#   lon           : numeric longitude
#   depth_var     : one of the supported soil temp variables
#   forecast_days : integer, usually 14
#
# Returns:
#   tibble(datetime, temp_c, source = "Forecast")
############################################################

fetch_openmeteo_forecast <- function(lat,
                                     lon,
                                     depth_var = "soil_temperature_6cm",
                                     forecast_days = 14) {
  
  depth_var <- validate_soil_depth_var(depth_var)
  
  forecast_days <- as.integer(forecast_days)
  
  if (is.na(forecast_days) || forecast_days < 1) {
    stop("forecast_days must be a positive integer.")
  }
  
  # Open-Meteo forecast docs support up to 16 days
  if (forecast_days > 16) {
    stop("forecast_days cannot exceed 16 for the Open-Meteo forecast endpoint.")
  }
  
  req <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      hourly = depth_var,
      timezone = "auto",
      forecast_days = forecast_days
    )

  js <- perform_json_request(req)
  
  parse_openmeteo_hourly(
    js = js,
    depth_var = depth_var,
    source_label = "Forecast"
  )
}

############################################################
# Convenience wrapper:
# fetch history + forecast and merge to one hourly table
#
# Args:
#   lat
#   lon
#   planting_date
#   depth_var
#   forecast_days
#
# Returns:
#   tibble(datetime, temp_c, source)
############################################################

fetch_openmeteo_timeseries <- function(lat,
                                       lon,
                                       planting_date,
                                       depth_var = "soil_temperature_6cm",
                                       forecast_days = 14) {
  
  planting_date <- as.character(as.Date(planting_date))
  today_date <- as.character(Sys.Date())
  
  hist <- fetch_openmeteo_history(
    lat = lat,
    lon = lon,
    start_date = planting_date,
    end_date = today_date,
    depth_var = depth_var
  )
  
  fcst <- fetch_openmeteo_forecast(
    lat = lat,
    lon = lon,
    depth_var = depth_var,
    forecast_days = forecast_days
  )
  
  bind_rows(hist, fcst) %>%
    arrange(datetime) %>%
    distinct(datetime, source, .keep_all = TRUE)
}

############################################################
# Fetch gridded Open-Meteo air temperature for early blight
############################################################

fetch_openmeteo_air_history <- function(lat,
                                        lon,
                                        start_date,
                                        end_date) {
  start_date <- as.character(as.Date(start_date))
  end_date   <- as.character(as.Date(end_date))

  req <- request("https://historical-forecast-api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = start_date,
      end_date = end_date,
      hourly = "temperature_2m",
      timezone = "auto"
    )

  js <- perform_json_request(req)
  parse_openmeteo_air_hourly(js, source_label = "Observed")
}

fetch_openmeteo_air_forecast <- function(lat,
                                         lon,
                                         forecast_days = 14) {
  forecast_days <- as.integer(forecast_days)

  if (is.na(forecast_days) || forecast_days < 1) {
    stop("forecast_days must be a positive integer.")
  }

  if (forecast_days > 16) {
    stop("forecast_days cannot exceed 16 for the Open-Meteo forecast endpoint.")
  }

  req <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      hourly = "temperature_2m",
      timezone = "auto",
      forecast_days = forecast_days
    )

  js <- perform_json_request(req)
  parse_openmeteo_air_hourly(js, source_label = "Forecast")
}

parse_openmeteo_air_hourly <- function(js, source_label) {
  if (is.null(js$hourly) || is.null(js$hourly$time) || is.null(js$hourly$temperature_2m)) {
    stop("Open-Meteo response is missing hourly air temperature data.")
  }

  tz_out <- if (!is.null(js$timezone)) js$timezone else "UTC"
  time_chr <- gsub("T", " ", js$hourly$time, fixed = TRUE)

  tibble(
    datetime = lubridate::ymd_hms(
      time_chr,
      tz = tz_out,
      quiet = TRUE,
      truncated = 1
    ),
    temp_c = as.numeric(js$hourly$temperature_2m),
    source = source_label
  )
}

fetch_openmeteo_air_timeseries <- function(lat,
                                           lon,
                                           planting_date,
                                           forecast_days = 14) {
  planting_date <- as.character(as.Date(planting_date))
  today_date <- as.character(Sys.Date())

  hist <- fetch_openmeteo_air_history(
    lat = lat,
    lon = lon,
    start_date = planting_date,
    end_date = today_date
  )

  fcst <- fetch_openmeteo_air_forecast(
    lat = lat,
    lon = lon,
    forecast_days = forecast_days
  )

  bind_rows(hist, fcst) %>%
    arrange(datetime) %>%
    distinct(datetime, source, .keep_all = TRUE)
}

forecast_rows_after <- function(df, reference_date = Sys.Date()) {
  if (!all(c("datetime", "temp_c", "source") %in% names(df))) {
    stop("forecast_rows_after() requires columns: datetime, temp_c, source")
  }

  df %>%
    mutate(date = as.Date(datetime)) %>%
    filter(date > as.Date(reference_date)) %>%
    select(datetime, temp_c, source)
}
