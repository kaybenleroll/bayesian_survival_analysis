library(tidyverse)
library(lubridate)

#' Determine policy status at a given date
#'
#' This function takes a tibble of policy data and a reference date,
#' then determines the status of each policy at that date based on:
#' - policy_startdate: when the policy became active
#' - policy_enddate: when the policy term ends
#' - policy_status: current status (e.g., 'lapsed', 'active', etc.)
#' - policy_statuschangedate: when the status changed
#'
#' @param policy_data A tibble containing policy information with columns:
#'   - policy_startdate (Date): Policy start date
#'   - policy_enddate (Date): Policy end date
#'   - policy_status (character): Current policy status
#'   - policy_statuschangedate (Date): Date when status changed
#' @param reference_date A Date object representing the date at which to check status
#'
#' @return A tibble with an additional column 'status_at_reference_date' indicating:
#'   - 'not_started': Policy hasn't started yet
#'   - 'active': Policy is active and in force
#'   - 'expired': Policy term has ended
#'   - 'lapsed': Policy has lapsed before the reference date
#'
determine_policy_status <- function(policy_data, reference_date) {

  # Validate inputs
  if (!is.data.frame(policy_data)) {
    stop("policy_data must be a data frame or tibble")
    }

  if (!inherits(reference_date, "Date")) {
    stop("reference_date must be a Date object")
    }

  required_cols <- c("policy_startdate", "policy_enddate", "policy_status", "policy_statuschangedate")
  missing_cols <- setdiff(required_cols, names(policy_data))

  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }

  # Ensure date columns are Date objects
  policy_data <- policy_data |>
    mutate(
      across(
        c(policy_startdate, policy_enddate, policy_statuschangedate),
        as.Date
        )
      )

  # Determine status at reference date
  updated_tbl <- policy_data |>
    mutate(
      status_at_reference_date = case_when(
        # Policy hasn't started yet
        (reference_date < policy_startdate) ~
          "not_started",

        # Policy has expired (term ended)
        (reference_date > policy_enddate) ~
          "completed",

        # Policy has lapsed and the lapse occurred before or on the reference date
        ((policy_status == "lapsed") & (policy_statuschangedate <= reference_date)) ~
          "lapsed",

        # Policy is within term and either not lapsed or lapsed after reference date
        ((reference_date >= policy_startdate) & (reference_date <= policy_enddate)) ~
          "inforce",

        # Default case (shouldn't occur with proper data)
        TRUE ~ "unknown"
        ),

        weeks_to_refdate    = difftime(
          reference_date,          policy_startdate, units = "weeks"
          ) |> as.numeric(),
        weeks_to_statusdate = difftime(
          policy_statuschangedate, policy_startdate, units = "weeks"
          ) |> as.numeric(),

        policy_lifetime = if_else(
          policy_status == "inforce",
          weeks_to_refdate,
          weeks_to_statusdate
          )
      )

  return(updated_tbl)
}

