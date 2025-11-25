#' Extract baseline hazard from a brmsfit Cox model
#'
#' This function extracts the baseline hazard function from a brmsfit object
#' that uses the cox() family. It returns a tidy tibble with time points,
#' draw IDs, and corresponding baseline hazard values.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param summary Should the function return summary statistics (mean, median, CI)?
#'   If FALSE (default), returns all draws. If TRUE, returns summary statistics.
#'
#' @return A tibble with columns:
#'   - time: Time points at which baseline hazard is evaluated
#'   - draw_id: MCMC draw identifier (omitted if summary = TRUE)
#'   - baseline_hazard: Baseline hazard value at each time point
#'   - mean_hazard: Mean baseline hazard (only if summary = TRUE)
#'   - median_hazard: Median baseline hazard (only if summary = TRUE)
#'   - lower_ci: Lower 95% credible interval (only if summary = TRUE)
#'   - upper_ci: Upper 95% credible interval (only if summary = TRUE)
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model with brms
#' cox_fit <- brm(
#'   time | cens(censored) ~ covariate1 + covariate2,
#'   data = data_tbl,
#'   family = cox()
#' )
#'
#' # Extract baseline hazard for all draws
#' hazard_draws_tbl <- extract_baseline_hazard(cox_fit)
#'
#' # Extract baseline hazard summary
#' hazard_summary_tbl <- extract_baseline_hazard(cox_fit, summary = TRUE)
#' }
#'
extract_baseline_hazard <- function(brmsfit_obj, summary = FALSE) {

  # Validate input
  if (!inherits(brmsfit_obj, "brmsfit")) {
    stop("brmsfit_obj must be a brmsfit object")
    }

  # Check if model uses cox() family
  model_family <- family(brmsfit_obj)$family
  if (model_family != "cox") {
    stop(
      glue::glue(
        "Model must use cox() family. Current family: {model_family}"
        )
      )
    }

  # Extract posterior draws
  post <- as_draws_df(brmsfit_obj)

  # Extract spline coefficients (sbhaz)
  sbhaz_coefs <- post |>
    tidy_draws() |>
    select(starts_with("sbhaz")) |>
    as.matrix()

  # Get the M-spline basis matrix stored in the brmsfit object
  # This is the actual basis matrix used during model fitting
  basis_matrix <- brmsfit_obj$basis$dpars$mu$bhaz$basis_matrix

  # Get time points from the original data
  # These correspond to the rows of the basis matrix
  resp_terms <- terms(formula(brmsfit_obj)$formula)
  resp_vars <- all.vars(resp_terms[[2]])
  time_var <- resp_vars[1]
  time_points <- brmsfit_obj$data[[time_var]]

  # Base R matrix multiplication
  # baseline_hazard = basis_matrix %*% t(sbhaz_coefs)
  # Dimensions: [n_obs x n_draws]
  # Each row corresponds to a time point from the original data
  # sbhaz represents the actual hazard coefficients (not log hazard)
  baseline_hazard_matrix <- as.matrix(basis_matrix) %*% t(sbhaz_coefs)

  # Convert to tidy format
  # Matrix is [n_obs x n_draws]
  hazard_tbl <- tibble(
    time            = rep(time_points, times = ncol(baseline_hazard_matrix)),
    draw_id         = rep(seq_len(ncol(baseline_hazard_matrix)), each = length(time_points)),
    baseline_hazard = as.vector(baseline_hazard_matrix)
    )

  # Return summary statistics if requested
  if (summary) {
    hazard_summary_tbl <- hazard_tbl |>
      group_by(time) |>
      summarise(
        mean_hazard   = mean(baseline_hazard),
        median_hazard = median(baseline_hazard),
        lower_ci      = quantile(baseline_hazard, 0.025),
        upper_ci      = quantile(baseline_hazard, 0.975),
        .groups       = "drop"
        )

    return(hazard_summary_tbl)
    }

  return(hazard_tbl)
}


#' Extract cumulative baseline hazard from a brmsfit Cox model
#'
#' This function computes the cumulative baseline hazard function from a
#' brmsfit object that uses the cox() family. It returns a tidy tibble with
#' time points, draw IDs, and corresponding cumulative baseline hazard values.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param summary Should the function return summary statistics (mean, median, CI)?
#'   If FALSE (default), returns all draws. If TRUE, returns summary statistics.
#'
#' @return A tibble with columns:
#'   - time: Time points at which cumulative baseline hazard is evaluated
#'   - draw_id: MCMC draw identifier (omitted if summary = TRUE)
#'   - cumulative_hazard: Cumulative baseline hazard value at each time point
#'   - mean_cumhaz: Mean cumulative baseline hazard (only if summary = TRUE)
#'   - median_cumhaz: Median cumulative baseline hazard (only if summary = TRUE)
#'   - lower_ci: Lower 95% credible interval (only if summary = TRUE)
#'   - upper_ci: Upper 95% credible interval (only if summary = TRUE)
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model with brms
#' cox_fit <- brm(
#'   time | cens(censored) ~ covariate1 + covariate2,
#'   data = data_tbl,
#'   family = cox()
#' )
#'
#' # Extract cumulative baseline hazard for all draws
#' cumhaz_draws_tbl <- extract_cumulative_hazard(cox_fit)
#'
#' # Extract cumulative baseline hazard summary
#' cumhaz_summary_tbl <- extract_cumulative_hazard(cox_fit, summary = TRUE)
#' }
#'
extract_cumulative_hazard <- function(brmsfit_obj, summary = FALSE) {

  # Get baseline hazard for each observation and draw
  hazard_tbl <- extract_baseline_hazard(brmsfit_obj, summary = FALSE)

  # Calculate cumulative hazard by draw
  # For each draw, compute the cumulative sum of hazards over time
  cumhaz_tbl <- hazard_tbl |>
    arrange(draw_id, time) |>
    group_by(draw_id) |>
    mutate(
      # Approximate cumulative hazard as cumulative sum
      # More accurate would be trapezoidal integration, but this is simpler
      cumulative_hazard = cumsum(baseline_hazard)
      ) |>
    ungroup() |>
    select(time, draw_id, cumulative_hazard)

  # Return summary statistics if requested
  if (summary) {
    cumhaz_summary_tbl <- cumhaz_tbl |>
      group_by(time) |>
      summarise(
        mean_cumhaz   = mean(cumulative_hazard),
        median_cumhaz = median(cumulative_hazard),
        lower_ci      = quantile(cumulative_hazard, 0.025),
        upper_ci      = quantile(cumulative_hazard, 0.975),
        .groups       = "drop"
        )

    return(cumhaz_summary_tbl)
    }

  return(cumhaz_tbl)
}


#' Extract baseline survival function from a brmsfit Cox model
#'
#' This function computes the baseline survival function from a brmsfit object
#' that uses the cox() family. The baseline survival is calculated as
#' S_0(t) = exp(-H_0(t)) where H_0(t) is the cumulative baseline hazard.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param summary Should the function return summary statistics (mean, median, CI)?
#'   If FALSE (default), returns all draws. If TRUE, returns summary statistics.
#'
#' @return A tibble with columns:
#'   - time: Time points at which baseline survival is evaluated
#'   - draw_id: MCMC draw identifier (omitted if summary = TRUE)
#'   - baseline_survival: Baseline survival probability at each time point
#'   - mean_survival: Mean baseline survival (only if summary = TRUE)
#'   - median_survival: Median baseline survival (only if summary = TRUE)
#'   - lower_ci: Lower 95% credible interval (only if summary = TRUE)
#'   - upper_ci: Upper 95% credible interval (only if summary = TRUE)
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model with brms
#' cox_fit <- brm(
#'   time | cens(censored) ~ covariate1 + covariate2,
#'   data = data_tbl,
#'   family = cox()
#' )
#'
#' # Extract baseline survival for all draws
#' survival_draws_tbl <- extract_baseline_survival(cox_fit)
#'
#' # Extract baseline survival summary
#' survival_summary_tbl <- extract_baseline_survival(cox_fit, summary = TRUE)
#' }
#'
extract_baseline_survival <- function(brmsfit_obj, summary = FALSE) {

  # Get cumulative baseline hazard
  cumhaz_tbl <- extract_cumulative_hazard(brmsfit_obj, summary = FALSE)

  # Calculate baseline survival: S_0(t) = exp(-H_0(t))
  survival_tbl <- cumhaz_tbl |>
    mutate(
      baseline_survival = exp(-cumulative_hazard)
      ) |>
    select(time, draw_id, baseline_survival)

  # Return summary statistics if requested
  if (summary) {
    survival_summary_tbl <- survival_tbl |>
      group_by(time) |>
      summarise(
        mean_survival   = mean(baseline_survival),
        median_survival = median(baseline_survival),
        lower_ci        = quantile(baseline_survival, 0.025),
        upper_ci        = quantile(baseline_survival, 0.975),
        .groups         = "drop"
        )

    return(survival_summary_tbl)
    }

  return(survival_tbl)
}


#' Plot baseline hazard from a brmsfit Cox model
#'
#' This function creates a ggplot visualization of the baseline hazard function
#' from a brmsfit Cox model, showing the posterior uncertainty.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param show_draws Number of individual draws to show (default: 50).
#'   Set to 0 to show only summary ribbon.
#' @param alpha_draws Alpha transparency for individual draws (default: 0.1)
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model with brms
#' cox_fit <- brm(
#'   time | cens(censored) ~ covariate1 + covariate2,
#'   data = data_tbl,
#'   family = cox()
#' )
#'
#' # Plot baseline hazard
#' plot_baseline_hazard(cox_fit)
#' }
#'
plot_baseline_hazard <- function(
    brmsfit_obj,
    show_draws = 50,
    alpha_draws = 0.1
  ) {

  # Get summary statistics
  hazard_summary_tbl <- extract_baseline_hazard(brmsfit_obj, summary = TRUE)

  # Start with base plot
  p <- ggplot(hazard_summary_tbl, aes(x = time))

  # Add individual draws if requested
  if (show_draws > 0) {
    hazard_draws_tbl <- extract_baseline_hazard(
      brmsfit_obj,
      summary = FALSE
      ) |>
      filter(draw_id %in% sample(unique(draw_id), min(show_draws, max(draw_id))))

    p <- p +
      geom_line(
        data  = hazard_draws_tbl,
        aes(y = baseline_hazard, group = draw_id),
        alpha = alpha_draws,
        color = "grey50"
        )
    }

  # Add summary statistics
  p <- p +
    geom_ribbon(
      aes(ymin = lower_ci, ymax = upper_ci),
      alpha = 0.3,
      fill  = "steelblue"
      ) +
    geom_line(
      aes(y = median_hazard),
      color = "steelblue",
      size  = 1
      ) +
    labs(
      title    = "Baseline Hazard Function",
      subtitle = "Median with 95% credible interval",
      x        = "Time",
      y        = "Baseline Hazard"
      ) +
    theme_minimal()

  return(p)
}


#' Plot baseline survival from a brmsfit Cox model
#'
#' This function creates a ggplot visualization of the baseline survival function
#' from a brmsfit Cox model, showing the posterior uncertainty.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param show_draws Number of individual draws to show (default: 50).
#'   Set to 0 to show only summary ribbon.
#' @param alpha_draws Alpha transparency for individual draws (default: 0.1)
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model with brms
#' cox_fit <- brm(
#'   time | cens(censored) ~ covariate1 + covariate2,
#'   data = data_tbl,
#'   family = cox()
#' )
#'
#' # Plot baseline survival
#' plot_baseline_survival(cox_fit)
#' }
#'
plot_baseline_survival <- function(
    brmsfit_obj,
    max_time  = 250,
    show_draws = 50,
    alpha_draws = 0.1
  ) {

  # Get summary statistics
  survival_summary_tbl <- extract_baseline_survival(
      brmsfit_obj,
      summary = TRUE
      ) |>
    filter(
      time <= max_time
      )

  # Start with base plot
  p <- ggplot(survival_summary_tbl, aes(x = time))

  # Add individual draws if requested
  if (show_draws > 0) {
    survival_draws_tbl <- extract_baseline_survival(
        brmsfit_obj,
        summary = FALSE
        ) |>
      filter(
        draw_id %in% sample(unique(draw_id), min(show_draws, max(draw_id))),
        time <= max_time
        )

    p <- p +
      geom_line(
        data  = survival_draws_tbl,
        aes(y = baseline_survival, group = draw_id),
        alpha = alpha_draws,
        color = "grey50"
        )
    }

  # Add summary statistics
  p <- p +
    geom_ribbon(
      aes(ymin = lower_ci, ymax = upper_ci),
      alpha = 0.3,
      fill  = "darkgreen"
      ) +
    geom_line(
      aes(y = median_survival),
      color = "darkgreen",
      size  = 1
      ) +
    labs(
      title    = "Baseline Survival Function",
      subtitle = "Median with 95% credible interval",
      x        = "Time",
      y        = "Baseline Survival Probability"
      ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()

  return(p)
}
