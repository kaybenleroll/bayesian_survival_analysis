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
#'   - dob_life1 (Date): Date of birth of primary policyholder
#' @param reference_date A Date object representing the date at which to check status
#'
#' @return A tibble with additional columns:
#'   - status_at_reference_date: 'not_started', 'inforce', 'completed', 'lapsed', or 'unknown'
#'   - weeks_to_refdate: Weeks from policy start to reference date
#'   - weeks_to_statusdate: Weeks from policy start to status change date
#'   - policy_lifetime: Weeks of policy life (to reference date if inforce, to status date if lapsed)
#'   - age_at_refdate: Age of policyholder at reference date (in years)
#'   - age_at_statusdate: Age of policyholder at status change date (in years)
#'   - age_at_observation: Age at appropriate date based on policy status
#'
determine_policy_status <- function(policy_data_tbl, reference_date) {

  # Validate inputs
  if (!is.data.frame(policy_data_tbl)) {
    stop("policy_data must be a data frame or tibble")
    }

  if (!inherits(reference_date, "Date")) {
    stop("reference_date must be a Date object")
    }

  required_cols <- c(
    "policy_startdate",
    "policy_enddate",
    "policy_status",
    "policy_statuschangedate",
    "dob_life1"
    )

  missing_cols <- setdiff(required_cols, names(policy_data_tbl))

  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }

  # Ensure date columns are Date objects
  policy_data_tbl <- policy_data_tbl |>
    mutate(
      across(
        c(policy_startdate, policy_enddate, policy_statuschangedate, dob_life1),
        as.Date
        )
      )

  # Determine status at reference date
  updated_tbl <- policy_data_tbl |>
    filter(policy_startdate < reference_date) |>
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
          ),

        # Calculate age at reference date
        age_at_refdate = time_length(
          difftime(reference_date, dob_life1),
          unit = "years"
          ),

        # Calculate age at status change date (for lapsed policies)
        age_at_statusdate = time_length(
          difftime(policy_statuschangedate, dob_life1),
          unit = "years"
          ),

        # Use appropriate age based on policy status
        age_at_observation = if_else(
          policy_status == "inforce",
          age_at_refdate,
          age_at_statusdate
          )
      )

  return(updated_tbl)
}


#' Extract Posterior Survival Probabilities from rstanarm Model
#'
#' Reshapes posterior survival probability matrix from rstanarm::posterior_survfit()
#' into a tidy long-format tibble suitable for analysis and visualization.
#' The input is typically a matrix with dimensions (draws × time_points).
#'
#' @param post_lst Posterior survival matrix from rstanarm::posterior_survfit()
#'   with attribute 'times' containing the time points
#'
#' @return Tibble with columns:
#'   - draw_id: Posterior draw identifier (iteration number)
#'   - idx: Original index position in the posterior matrix
#'   - value: Survival probability at this time point
#'   - time: Time value corresponding to this survival probability
#'
#' @examples
#' # Fit Cox model and extract posterior survival
#' fit <- stan_surv(Surv(time, event) ~ x1 + x2, data = data_tbl)
#' post_surv <- posterior_survfit(fit, newdata = newdata_tbl)
#'
#' # Convert to tidy format
#' post_surv_tbl <- extract_stansurv_posterior_survivals(post_surv)
#'
extract_stansurv_posterior_survivals <- function(post_lst) {
  time_vals <- post_lst |> attr('times')

  rep_count <- post_lst |> nrow()

  post_surv_tbl <- post_lst |>
    reshape2::melt() |>
    rename(
      draw_id = iterations,
      idx     = ids
    ) |>
    as_tibble() |>
    mutate(
      time = rep(time_vals, rep_count)
    )

  return(post_surv_tbl)
}


#' Interpolate Survival Probabilities to New Time Points
#'
#' Performs linear interpolation of survival curves to evaluate them at new
#' time points. This is useful when you need survival probabilities at specific
#' times not in the original posterior draws (e.g., monthly or weekly intervals).
#'
#' @param surv_data_tbl Tibble with survival data containing columns:
#'   - policy_id: Policy identifier
#'   - draw_id: Posterior draw identifier
#'   - time: Original time points
#'   - post_surv_prob: Survival probabilities at original times
#' @param new_times Numeric vector: New time points at which to evaluate survival
#'
#' @return Tibble with columns:
#'   - policy_id: Policy identifier
#'   - draw_id: Posterior draw identifier
#'   - interp_time: New interpolated time points
#'   - post_surv_prob: Interpolated survival probabilities
#'
#' @examples
#' # Interpolate survival to monthly intervals
#' monthly_times <- seq(0, 520, by = 4)  # Weeks to months
#' monthly_surv_tbl <- interpolate_survival_data(
#'   post_surv_tbl,
#'   new_times = monthly_times
#' )
#'
interpolate_survival_data <- function(surv_data_tbl, new_times) {
  interp_survdata_tbl <- surv_data_tbl |>
    reframe(
      interp_time    = new_times,
      post_surv_prob = approx(time, post_surv_prob, xout = new_times)$y,

      .by = c(policy_id, draw_id)
    )

  return(interp_survdata_tbl)
}


#' Extract Survival Time from Survival Curve Using Random Draw
#'
#' Implements inverse transform sampling to generate event times from a survival
#' curve. Given a uniform random draw u ~ U(0,1), finds the time t where S(t) = u.
#' This is the inverse of the survival CDF and is used for simulating event times.
#'
#' @param data_tbl Tibble with single survival curve containing columns:
#'   - interp_time: Time points (must be sorted)
#'   - post_surv_prob: Survival probabilities at those times
#' @param rng_draw Numeric: Random uniform draw between 0 and 1
#' @param min_time Numeric: Minimum time to return (floor value)
#'
#' @return Numeric: Simulated event time (or NA if no valid time exists)
#'
#' @examples
#' # Simulate lapse time from survival curve
#' surv_curve_tbl <- tibble(
#'   interp_time = seq(0, 520, by = 4),
#'   post_surv_prob = exp(-0.001 * interp_time)  # Exponential survival
#' )
#'
#' # Generate random lapse time
#' u <- runif(1)
#' lapse_time <- extract_survival_time(surv_curve_tbl, u, min_time = 0)
#'
#' # Simulate many lapse times
#' lapse_times <- map_dbl(runif(1000), ~ extract_survival_time(surv_curve_tbl, .x, 0))
#'
extract_survival_time <- function(data_tbl, rng_draw, min_time) {
  valid_idx <- data_tbl$post_surv_prob > rng_draw

  if (!any(valid_idx)) {
    return(NA_real_)  # No valid times (u > all survival probabilities)
  }

  # Get the last TRUE index (highest time where surv_prob > u)
  max_idx <- max(which(valid_idx))

  lapse_time <- pmax(data_tbl$interp_time[max_idx], min_time)

  return(lapse_time)
}


