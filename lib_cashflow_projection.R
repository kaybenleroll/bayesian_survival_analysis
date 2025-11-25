#' Calculate Monthly Cashflows from Insurance Policy Book
#'
#' Takes a tibble of insurance policies and a vector of cashflow dates,
#' then calculates the expected premium cashflows at those dates,
#' accounting for when policies lapse or complete.
#'
#' @param policy_tbl A tibble containing policy information with columns:
#'   - policy_id (character): Unique policy identifier
#'   - policy_status (character): Current status ('inforce', 'lapsed', 'completed')
#'   - policy_startdate (Date): When the policy started
#'   - policy_enddate (Date): When the policy term ends
#'   - policy_statuschangedate (Date): When status changed (for lapsed policies)
#'   - prem_ape (numeric): Annual Premium Equivalent (annualized premium amount)
#'
#' @param cashflow_dates Vector of Date objects: Dates at which to calculate cashflows
#'
#' @return A tibble with columns:
#'   - cashflow_date (Date): Dates from cashflow_dates parameter
#'   - policy_id (character): Policy identifier
#'   - policy_status (character): Policy status
#'   - is_active (logical): Whether policy is paying premiums at this date
#'   - monthly_premium (numeric): Premium amount per period (prem_ape/12)
#'   - cashflow (numeric): Actual cashflow (monthly_premium if active, 0 if not)
#'
#' @examples
#' policy_tbl <- tibble(
#'   policy_id = c("P001", "P002", "P003"),
#'   policy_status = c("inforce", "lapsed", "completed"),
#'   policy_startdate = as.Date(c("2020-01-01", "2019-06-15", "2018-03-01")),
#'   policy_enddate = as.Date(c("2030-01-01", "2029-06-15", "2024-03-01")),
#'   policy_statuschangedate = as.Date(c(NA, "2023-08-15", "2024-03-01")),
#'   prem_ape = c(1200, 2400, 1800)
#' )
#'
#' # Generate monthly dates
#' cashflow_dates <- seq(
#'   as.Date("2024-01-01"),
#'   as.Date("2025-12-31"),
#'   by = "month"
#' )
#'
#' cashflows <- calculate_monthly_cashflows(policy_tbl, cashflow_dates)
#'
calculate_monthly_cashflows <- function(policy_tbl, cashflow_dates) {

  # Validate inputs
  required_cols <- c(
    "policy_id", "policy_status", "policy_startdate", "policy_enddate",
    "policy_statuschangedate", "prem_ape"
    )

  missing_cols <- setdiff(required_cols, names(policy_tbl))

  if (length(missing_cols) > 0) {
    stop(
      glue("Missing required columns: {paste(missing_cols, collapse = ', ')}")
      )
  }

  if (!inherits(cashflow_dates, "Date")) {
    stop("cashflow_dates must be a vector of Date objects")
  }

  if (length(cashflow_dates) == 0) {
    stop("cashflow_dates cannot be empty")
  }

  # Ensure date columns are Date objects
  policy_tbl <- policy_tbl |>
    mutate(
      across(
        c(policy_startdate, policy_enddate, policy_statuschangedate),
        as.Date
        )
      )

  # Create grid of all policy-cashflow date combinations
  cashflow_grid_tbl <- expand_grid(
      cashflow_date = cashflow_dates,
      policy_id     = policy_tbl$policy_id
      ) |>
    left_join(policy_tbl, by = "policy_id")

  # Determine if policy is active at each cashflow date
  cashflow_tbl <- cashflow_grid_tbl |>
    mutate(
      # Policy must have started by the cashflow date
      started_by_cashflow_date = policy_startdate <= cashflow_date,

      # For lapsed policies: cashflow date must be before lapse date
      # For completed policies: cashflow date must be before end date
      # For inforce policies: cashflow date must be before end date
      not_lapsed_at_cashflow = case_when(
        policy_status == "lapsed"    ~ cashflow_date < policy_statuschangedate,
        policy_status == "completed" ~ cashflow_date < policy_enddate,
        policy_status == "inforce"   ~ cashflow_date < policy_enddate,

        TRUE ~ FALSE
        ),

      # Policy is active if it has started and hasn't lapsed/completed
      is_active = started_by_cashflow_date & not_lapsed_at_cashflow,

      # Calculate monthly premium (APE / 12)
      monthly_premium = prem_ape / 12,

      # Actual cashflow is premium if active, 0 otherwise
      cashflow = if_else(is_active, monthly_premium, 0)
      ) |>
    select(
      cashflow_date, policy_id, policy_status, policy_startdate, policy_enddate,
      policy_statuschangedate, is_active, monthly_premium, cashflow
      )

  return(cashflow_tbl)
}


#' Summarize Monthly Cashflows by Period
#'
#' Aggregates policy-level cashflows to monthly totals with additional statistics
#'
#' @param cashflow_tbl Output from calculate_monthly_cashflows()
#'
#' @return A tibble with columns:
#'   - cashflow_date (Date): First day of month
#'   - total_cashflow (numeric): Sum of all cashflows in month
#'   - active_policies (integer): Number of active policies
#'   - inactive_policies (integer): Number of inactive policies
#'   - total_policies (integer): Total policies in book
#'
summarize_monthly_cashflows <- function(cashflow_tbl) {
  cashflow_tbl |>
    group_by(cashflow_date) |>
    summarise(
      total_cashflow       = sum(cashflow, na.rm = TRUE),
      active_policies      = sum(is_active, na.rm = TRUE),
      inactive_policies    = sum(!is_active, na.rm = TRUE),
      total_policies       = n(),
      mean_monthly_premium = mean(
        monthly_premium[is_active],
        na.rm = TRUE
        ),
      .groups = "drop"
      )
}


#' Calculate Cumulative Cashflows Over Time
#'
#' Computes running totals of cashflows and policy counts
#'
#' @param summary_tbl Output from summarize_monthly_cashflows()
#'
#' @return A tibble with cumulative columns added:
#'   - cumulative_cashflow (numeric): Running total of cashflows
#'   - cumulative_active (numeric): Running total of active policy-months
#'
calculate_cumulative_cashflows <- function(summary_tbl) {
  summary_tbl |>
    arrange(cashflow_date) |>
    mutate(
      cumulative_cashflow = cumsum(total_cashflow),
      cumulative_active = cumsum(active_policies)
      )
}


#' Visualize Monthly Cashflows
#'
#' Creates a plot showing cashflows and active policy counts over time
#'
#' @param cashflow_summary_tbl Output from summarize_monthly_cashflows()
#'
#' @return A ggplot object
#'
plot_monthly_cashflows <- function(cashflow_summary_tbl) {
  library(cowplot)
  library(scales)

  # Cashflow plot
  cashflow_plot <- ggplot(
    cashflow_summary_tbl,
    aes(x = cashflow_date, y = total_cashflow)
    ) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    labs(
      title = "Monthly Premium Cashflows",
      x = "Month",
      y = "Total Monthly Premium",
      caption = "Source: Policy book data"
      ) +
    scale_y_continuous(labels = label_comma()) +
    theme_cowplot()

  # Active policies plot
  policies_plot <- ggplot(
    cashflow_summary_tbl,
    aes(x = cashflow_date, y = active_policies)
    ) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_point(size = 2, color = "darkgreen") +
    labs(
      title = "Active Policies Over Time",
      x = "Month",
      y = "Number of Active Policies"
      ) +
    scale_y_continuous(labels = label_comma()) +
    theme_cowplot()

  # Combine plots
  plot_grid(
    cashflow_plot,
    policies_plot,
    ncol = 1,
    align = "v"
    )
}


