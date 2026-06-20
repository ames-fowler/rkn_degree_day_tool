############################################################
# API Hardening Helpers
# File: R/api_hardening.R
############################################################

library(httr2)

perform_json_request <- function(req,
                                 user_agent = "Potato-predictive-tools-Shiny-app/0.3",
                                 timeout_seconds = 20,
                                 max_retries = 3) {
  req %>%
    req_user_agent(user_agent) %>%
    req_timeout(timeout_seconds) %>%
    req_retry(
      max_tries = max_retries,
      backoff = function(attempt) min(8, 0.75 * 2^(attempt - 1)),
      is_transient = function(resp) {
        status <- resp_status(resp)
        status %in% c(408, 425, 429, 500, 502, 503, 504)
      }
    ) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
}
