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
      policy_startdate = as.Date(policy_startdate),
      policy_enddate = as.Date(policy_enddate),
      policy_statuschangedate = as.Date(policy_statuschangedate)
    )
  
  # Determine status at reference date
  result <- policy_data |>
    mutate(
      status_at_reference_date = case_when(
        # Policy hasn't started yet
        reference_date < policy_startdate ~ "not_started",
        
        # Policy has expired (term ended)
        reference_date > policy_enddate ~ "expired",
        
        # Policy has lapsed and the lapse occurred before or on the reference date
        policy_status == "lapsed" & policy_statuschangedate <= reference_date ~ "lapsed",
        
        # Policy is within term and either not lapsed or lapsed after reference date
        reference_date >= policy_startdate & reference_date <= policy_enddate ~ "active",
        
        # Default case (shouldn't occur with proper data)
        TRUE ~ "unknown"
      )
    )
  
  return(result)
}

#' Count policies by status at a reference date
#'
#' @param policy_data A tibble containing policy information
#' @param reference_date A Date object representing the date at which to check status
#'
#' @return A tibble with status counts
#'
count_policy_status <- function(policy_data, reference_date) {
  
  status_data <- determine_policy_status(policy_data, reference_date)
  
  status_counts <- status_data |>
    count(status_at_reference_date, name = "count") |>
    arrange(desc(count))
  
  return(status_counts)
}

#' Get in-force policies at a reference date
#'
#' @param policy_data A tibble containing policy information
#' @param reference_date A Date object representing the date at which to check status
#'
#' @return A tibble containing only policies that are active/in-force at the reference date
#'
get_inforce_policies <- function(policy_data, reference_date) {
  
  status_data <- determine_policy_status(policy_data, reference_date)
  
  inforce_policies <- status_data |>
    filter(status_at_reference_date == "active")
  
  return(inforce_policies)
}

# Example usage and testing function
test_policy_status_function <- function() {
  
  # Create sample data
  sample_data <- tibble(
    policy_id = 1:6,
    policy_startdate = as.Date(c("2020-01-01", "2021-06-01", "2022-01-01", 
                                 "2023-01-01", "2019-01-01", "2024-01-01")),
    policy_enddate = as.Date(c("2025-01-01", "2026-06-01", "2027-01-01", 
                               "2028-01-01", "2024-01-01", "2029-01-01")),
    policy_status = c("active", "lapsed", "active", "lapsed", "active", "active"),
    policy_statuschangedate = as.Date(c("2020-01-01", "2022-03-01", "2022-01-01", 
                                        "2023-06-01", "2019-01-01", "2024-01-01"))
  )
  
  # Test with a reference date
  reference_date <- as.Date("2022-12-31")
  
  cat("Sample data:\n")
  print(sample_data)
  
  cat("\nPolicy status at", as.character(reference_date), ":\n")
  result <- determine_policy_status(sample_data, reference_date)
  print(result)
  
  cat("\nStatus counts:\n")
  counts <- count_policy_status(sample_data, reference_date)
  print(counts)
  
  cat("\nIn-force policies:\n")
  inforce <- get_inforce_policies(sample_data, reference_date)
  print(inforce)
}

# Uncomment the line below to run the test
# test_policy_status_function()
