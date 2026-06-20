############################################################
# CoAgMET API Helpers
# File: R/coagmet_api.R
############################################################

library(dplyr)
library(httr2)
library(lubridate)
library(readr)
library(tibble)

coagmet_station_registry_path <- function() {
  here::here("data", "coagmet_active_sites.csv")
}

read_coagmet_stations <- function(active_only = TRUE) {
  path <- coagmet_station_registry_path()

  if (!file.exists(path)) {
    stop("Missing CoAgMET station registry: ", path)
  }

  stations <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      elevation_m = as.numeric(elevation_m),
      end_date = coalesce(end_date, ""),
      label = paste0(name, " (", site_id, ")")
    ) %>%
    filter(!is.na(latitude), !is.na(longitude))

  if (isTRUE(active_only)) {
    stations <- stations %>% filter(end_date == "")
  }

  stations %>% arrange(name, site_id)
}

coagmet_station_choices <- function(active_only = TRUE) {
  stations <- read_coagmet_stations(active_only = active_only)
  stats::setNames(stations$site_id, stations$label)
}

fetch_coagmet_daily_weather <- function(station_id,
                                        start_date,
                                        end_date = Sys.Date(),
                                        units = "m") {
  station_id <- trimws(station_id)
  start_date <- as.character(as.Date(start_date))
  end_date <- as.character(as.Date(end_date))

  req <- request(
    paste0("https://coagmet.colostate.edu/data/daily/", station_id, ".json")
  ) %>%
    req_url_query(
      from = start_date,
      to = end_date,
      units = units,
      dateFmt = "iso"
    ) %>%
    req_user_agent("Potato-predictive-tools-Shiny-app/0.2")

  perform_json_request(req)
}

missing_to_na <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[out == -999] <- NA_real_
  out
}

coagmet_daily_to_temperature <- function(payload,
                                         measure = c("air_minmax_mean", "soil_5cm", "soil_15cm")) {
  measure <- match.arg(measure)

  if (is.null(payload$time)) {
    stop("CoAgMET daily payload is missing time.")
  }

  dates <- as.Date(payload$time)

  temp_c <- switch(
    measure,
    air_minmax_mean = {
      (missing_to_na(payload$tMax) + missing_to_na(payload$tMin)) / 2
    },
    soil_5cm = {
      (missing_to_na(payload$st5Max) + missing_to_na(payload$st5Min)) / 2
    },
    soil_15cm = {
      (missing_to_na(payload$st15Max) + missing_to_na(payload$st15Min)) / 2
    }
  )

  tibble(
    datetime = as.POSIXct(paste(dates, "12:00:00"), tz = "MST"),
    temp_c = temp_c,
    source = "Observed",
    date = dates
  ) %>%
    filter(!is.na(datetime), !is.na(temp_c))
}

fetch_coagmet_temperature_timeseries <- function(station_id,
                                                 planting_date,
                                                 measure = "air_minmax_mean") {
  payload <- fetch_coagmet_daily_weather(
    station_id = station_id,
    start_date = planting_date,
    end_date = Sys.Date(),
    units = "m"
  )

  coagmet_daily_to_temperature(payload, measure = measure)
}

coagmet_measure_to_projection_depth <- function(measure) {
  switch(
    measure,
    soil_5cm = "soil_temperature_6cm",
    soil_15cm = "soil_temperature_18cm",
    air_minmax_mean = NA_character_,
    stop("Unsupported CoAgMET measure for projection: ", measure)
  )
}

fetch_coagmet_with_projection <- function(station_row,
                                          planting_date,
                                          measure = "air_minmax_mean",
                                          forecast_days = 14) {
  required_cols <- c("site_id", "latitude", "longitude")
  missing_cols <- setdiff(required_cols, names(station_row))

  if (length(missing_cols) > 0) {
    stop(
      "Station row is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (nrow(station_row) < 1) {
    stop("Station row is empty.")
  }

  station_id <- station_row$site_id[[1]]
  lat <- as.numeric(station_row$latitude[[1]])
  lon <- as.numeric(station_row$longitude[[1]])

  if (is.na(lat) || is.na(lon)) {
    stop("Station row has invalid latitude or longitude.")
  }

  observed <- fetch_coagmet_temperature_timeseries(
    station_id = station_id,
    planting_date = planting_date,
    measure = measure
  )

  projection <- if (identical(measure, "air_minmax_mean")) {
    fetch_openmeteo_air_forecast(
      lat = lat,
      lon = lon,
      forecast_days = forecast_days
    )
  } else {
    fetch_openmeteo_forecast(
      lat = lat,
      lon = lon,
      depth_var = coagmet_measure_to_projection_depth(measure),
      forecast_days = forecast_days
    )
  }

  bind_rows(
    observed %>% select(datetime, temp_c, source),
    forecast_rows_after(projection)
  ) %>%
    arrange(datetime)
}

coagmet_measure_label <- function(measure) {
  switch(
    measure,
    air_minmax_mean = "CoAgMET air temperature: (Tmax + Tmin) / 2",
    soil_5cm = "CoAgMET soil temperature: 5 cm mean of daily max/min",
    soil_15cm = "CoAgMET soil temperature: 15 cm mean of daily max/min",
    measure
  )
}
