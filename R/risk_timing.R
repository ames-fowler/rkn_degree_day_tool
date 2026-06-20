############################################################
# Risk Timing Helpers
# File: R/risk_timing.R
############################################################

library(dplyr)
library(tibble)

build_threshold_table <- function(labels, thresholds) {
  tibble(
    label = labels,
    threshold = as.numeric(thresholds)
  )
}

summarize_threshold_timing <- function(df_dd, thresholds, reference_date = Sys.Date()) {
  if (is.null(df_dd) || nrow(df_dd) == 0) {
    return(tibble())
  }

  current <- df_dd %>%
    arrange(date) %>%
    filter(date <= as.Date(reference_date)) %>%
    filter(!is.na(cum_dd)) %>%
    slice_tail(n = 1)

  if (nrow(current) == 0) {
    return(tibble())
  }

  current_dd <- current$cum_dd[[1]]
  reference_date <- as.Date(reference_date)

  thresholds %>%
    rowwise() %>%
    mutate(
      crossing_date = {
        hit <- df_dd %>%
          arrange(date) %>%
          filter(cum_dd >= threshold) %>%
          slice_head(n = 1)

        if (nrow(hit) == 0) {
          as.Date(NA)
        } else {
          as.Date(hit$date[[1]])
        }
      },
      days_from_today = if (is.na(crossing_date)) {
        NA_integer_
      } else {
        as.integer(crossing_date - reference_date)
      },
      remaining_dd = max(0, threshold - current_dd),
      status = case_when(
        is.na(crossing_date) ~ paste0("not reached; ", round(remaining_dd, 1), " DD remaining"),
        days_from_today < 0 ~ paste0("reached ", abs(days_from_today), " days ago"),
        days_from_today == 0 ~ "reached today",
        TRUE ~ paste0("expected in ", days_from_today, " days")
      )
    ) %>%
    ungroup() %>%
    mutate(
      threshold = round(threshold, 1),
      current_dd = round(current_dd, 1),
      remaining_dd = round(remaining_dd, 1)
    )
}
