############################################################
# Plot Label Helpers
# File: R/plot_labels.R
############################################################

format_weather_source <- function(provider, source) {
  provider <- ifelse(is.na(provider) | !nzchar(provider), "Weather API", provider)
  source <- ifelse(is.na(source) | !nzchar(source), "Observed", source)
  paste(provider, tolower(source))
}

format_map_location_label <- function(location) {
  if (is.null(location)) {
    return("Map point")
  }

  paste0(
    if (!is.null(location$label)) location$label else "Map point",
    " (",
    round(location$lat, 5),
    ", ",
    round(location$lon, 5),
    ")"
  )
}

format_station_location_label <- function(station) {
  if (is.null(station) || nrow(station) == 0) {
    return("CoAgMET station")
  }

  paste0(
    station$name[[1]],
    " (",
    station$site_id[[1]],
    "; ",
    round(station$latitude[[1]], 5),
    ", ",
    round(station$longitude[[1]], 5),
    ")"
  )
}
