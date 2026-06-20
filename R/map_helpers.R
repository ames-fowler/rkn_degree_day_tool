############################################################
# Map Helper Functions
# File: R/map_helpers.R
#
# Purpose:
#   Helper functions for:
#     - parsing pasted lat,lon input
#     - geocoding address strings
#     - extracting points / centroids from drawn features
############################################################

library(httr2)
library(jsonlite)
library(sf)

############################################################
# Safely coerce to numeric
############################################################

safe_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || all(is.na(out))) {
    return(NA_real_)
  }
  out
}

############################################################
# Parse a string like:
#   "40.5853,-105.0844"
#   "40.5853, -105.0844"
#
# Returns:
#   list(lat=..., lon=...) or NULL
############################################################

parse_latlon <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) {
    return(NULL)
  }
  
  cleaned <- gsub("\\s+", "", txt)
  parts <- strsplit(cleaned, ",")[[1]]
  
  if (length(parts) != 2) {
    return(NULL)
  }
  
  lat <- safe_num(parts[1])
  lon <- safe_num(parts[2])
  
  if (is.na(lat) || is.na(lon)) {
    return(NULL)
  }
  
  if (lat < -90 || lat > 90 || lon < -180 || lon > 180) {
    return(NULL)
  }
  
  list(lat = lat, lon = lon)
}

############################################################
# Geocode using Nominatim
#
# Returns:
#   list(lat=..., lon=..., label=...) or NULL
#
# Note:
#   Nominatim requests should include a user-agent.
############################################################

geocode_nominatim <- function(query) {
  if (is.null(query) || !nzchar(trimws(query))) {
    return(NULL)
  }
  
  url <- paste0(
    "https://nominatim.openstreetmap.org/search?",
    "format=jsonv2",
    "&limit=1",
    "&q=", URLencode(query, reserved = TRUE)
  )
  
  resp <- request(url) %>%
    req_user_agent("Potato-predictive-tools-Shiny-app/0.3") %>%
    req_timeout(20) %>%
    req_retry(max_tries = 3) %>%
    req_perform()

  txt <- resp_body_string(resp)
  js <- fromJSON(txt, simplifyDataFrame = TRUE)
  
  if (length(js) == 0 || nrow(js) == 0) {
    return(NULL)
  }
  
  list(
    lat = as.numeric(js$lat[1]),
    lon = as.numeric(js$lon[1]),
    label = js$display_name[1]
  )
}

############################################################
# Extract point or centroid from a Leaflet draw feature
#
# Supported geometry types:
#   - Point
#   - Polygon
#
# In many leaflet-draw cases, rectangles also come through
# as Polygon geometries, so handling Polygon is usually enough.
#
# Returns:
#   list(lat=..., lon=..., type=...) or NULL
############################################################

extract_drawn_centroid <- function(feature) {
  if (is.null(feature) || is.null(feature$geometry) || is.null(feature$geometry$type)) {
    return(NULL)
  }
  
  gtype <- feature$geometry$type
  
  ##########################################################
  # Point
  ##########################################################
  if (gtype == "Point") {
    coords <- feature$geometry$coordinates
    
    return(list(
      lon = as.numeric(coords[[1]]),
      lat = as.numeric(coords[[2]]),
      type = "Point"
    ))
  }
  
  ##########################################################
  # Polygon
  #
  # Leaflet draw polygons typically come as:
  # feature$geometry$coordinates[[1]] = list of lon/lat pairs
  ##########################################################
  if (gtype == "Polygon") {
    ring <- do.call(
      rbind,
      lapply(feature$geometry$coordinates[[1]], function(x) {
        c(as.numeric(x[[1]]), as.numeric(x[[2]]))
      })
    )
    
    poly <- st_polygon(list(ring))
    sfc <- st_sfc(poly, crs = 4326)
    
    cent <- st_coordinates(st_centroid(sfc))
    
    return(list(
      lon = as.numeric(cent[1]),
      lat = as.numeric(cent[2]),
      type = "Polygon"
    ))
  }
  
  ##########################################################
  # Unsupported geometry
  ##########################################################
  NULL
}
