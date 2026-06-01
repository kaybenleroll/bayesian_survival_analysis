#' Extract baseline hazard strata from a brmsfit Cox model
#'
#' This function returns the baseline hazard strata configured via
#' `bhaz(gr = ...)` in a brms Cox model. Unstratified models return a single
#' row with the label `all`.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#'
#' @return A tibble with columns:
#'   - group_id: Integer stratum identifier used internally by brms
#'   - baseline_strata: Human-readable stratum label
#'
extract_baseline_strata <- function(brmsfit_obj) {

  validate_brms_cox_model(brmsfit_obj)

  standata_lst <- make_standata(
    formula(brmsfit_obj),
    data   = brmsfit_obj$data,
    family = family(brmsfit_obj)
    )

  if (!"Jgrbhaz" %in% names(standata_lst)) {
    return(
      tibble(
        group_id        = 1L,
        baseline_strata = "all"
        )
      )
    }

  group_id <- seq_len(standata_lst$ngrbhaz)
  group_labels <- family(brmsfit_obj)$bhaz$groups

  if (is.null(group_labels) || length(group_labels) == 0) {
    group_labels <- as.character(group_id)
    }

  tibble(
    group_id        = group_id,
    baseline_strata = as.character(group_labels[group_id])
    )
}


validate_brms_cox_model <- function(brmsfit_obj) {

  if (!inherits(brmsfit_obj, "brmsfit")) {
    stop("brmsfit_obj must be a brmsfit object")
    }

  model_family <- family(brmsfit_obj)$family
  if (model_family != "cox") {
    stop(
      glue::glue(
        "Model must use cox() family. Current family: {model_family}"
        )
      )
    }
}


extract_sbhaz_draws <- function(brmsfit_obj) {

  post_tbl <- as_draws_df(brmsfit_obj)
  sbhaz_names <- names(post_tbl)[grepl("^sbhaz(\\[|\\.)", names(post_tbl))]

  if (length(sbhaz_names) == 0) {
    stop("No sbhaz parameters found. Model may not include a flexible Cox baseline hazard.")
    }

  sbhaz_index_tbl <- tibble(parameter = sbhaz_names) |>
    mutate(
      bracket_group = stringr::str_match(parameter, "^sbhaz\\[([^,]+),(\\d+)\\]$")[, 2],
      bracket_basis = stringr::str_match(parameter, "^sbhaz\\[([^,]+),(\\d+)\\]$")[, 3],
      bracket_single = stringr::str_match(parameter, "^sbhaz\\[(\\d+)\\]$")[, 2],
      dot_group = stringr::str_match(parameter, "^sbhaz\\.([^.]+)\\.(\\d+)$")[, 2],
      dot_basis = stringr::str_match(parameter, "^sbhaz\\.([^.]+)\\.(\\d+)$")[, 3],
      dot_single = stringr::str_match(parameter, "^sbhaz\\.(\\d+)$")[, 2]
      ) |>
    mutate(
      group_key = case_when(
        !is.na(bracket_group) ~ bracket_group,
        !is.na(dot_group) ~ dot_group,
        TRUE ~ "all"
        ),
      basis_id = case_when(
        !is.na(bracket_basis) ~ as.integer(bracket_basis),
        !is.na(dot_basis) ~ as.integer(dot_basis),
        !is.na(bracket_single) ~ as.integer(bracket_single),
        !is.na(dot_single) ~ as.integer(dot_single),
        TRUE ~ NA_integer_
        )
      ) |>
    select(parameter, group_key, basis_id)

  if (any(is.na(sbhaz_index_tbl$basis_id))) {
    stop("Unable to parse sbhaz parameter indices from posterior draws.")
    }

  sbhaz_matrix <- post_tbl |>
    select(all_of(sbhaz_names)) |>
    as.matrix()

  sbhaz_index_tbl <- sbhaz_index_tbl |>
    arrange(group_key, basis_id)

  split(sbhaz_index_tbl$parameter, sbhaz_index_tbl$group_key) |>
    purrr::map(
      ~sbhaz_matrix[, .x, drop = FALSE]
      )
}


extract_brms_cox_baseline_tbl <- function(
    brmsfit_obj,
    basis_name,
    value_name,
    mean_name,
    median_name,
    summary = FALSE,
    strata = NULL
  ) {

  validate_brms_cox_model(brmsfit_obj)

  standata_lst <- make_standata(
    formula(brmsfit_obj),
    data   = brmsfit_obj$data,
    family = family(brmsfit_obj)
    )

  basis_matrix <- standata_lst[[basis_name]]
  if (is.null(basis_matrix)) {
    stop(glue::glue("Standata does not contain `{basis_name}`."))
    }

  resp_terms <- terms(formula(brmsfit_obj)$formula)
  resp_vars <- all.vars(resp_terms[[2]])
  time_var <- resp_vars[1]
  time_points <- brmsfit_obj$data[[time_var]]

  if ("Jgrbhaz" %in% names(standata_lst)) {
    stratum_id <- standata_lst$Jgrbhaz
  } else {
    stratum_id <- rep(1L, nrow(basis_matrix))
    }

  strata_tbl <- extract_baseline_strata(brmsfit_obj)

  if (!is.null(strata)) {
    unknown_strata <- setdiff(strata, strata_tbl$baseline_strata)
    if (length(unknown_strata) > 0) {
      stop(
        glue::glue(
          "Unknown baseline strata: {paste(unknown_strata, collapse = ', ')}"
          )
        )
      }

    strata_tbl <- strata_tbl |>
      filter(baseline_strata %in% strata)
    }

  sbhaz_draws_lst <- extract_sbhaz_draws(brmsfit_obj)

  if (length(sbhaz_draws_lst) != nrow(strata_tbl) && length(sbhaz_draws_lst) != 1) {
    stop("Mismatch between baseline hazard strata and sbhaz posterior draws.")
    }

  value_tbl <- strata_tbl |>
    mutate(
      draws = purrr::map(
        group_id,
        ~{
          sbhaz_draw_matrix <- if (length(sbhaz_draws_lst) == 1) {
            sbhaz_draws_lst[[1]]
          } else if (strata_tbl$baseline_strata[strata_tbl$group_id == .x] %in% names(sbhaz_draws_lst)) {
            sbhaz_draws_lst[[strata_tbl$baseline_strata[strata_tbl$group_id == .x]]]
          } else if (as.character(.x) %in% names(sbhaz_draws_lst)) {
            sbhaz_draws_lst[[as.character(.x)]]
          } else {
            sbhaz_draws_lst[[.x]]
          }

          obs_index <- which(stratum_id == .x)
          value_matrix <- as.matrix(basis_matrix[obs_index, , drop = FALSE]) %*% t(sbhaz_draw_matrix)

          tibble(
            time    = rep(time_points[obs_index], times = ncol(value_matrix)),
            draw_id = rep(seq_len(ncol(value_matrix)), each = length(obs_index)),
            value   = as.vector(value_matrix)
            )
          }
        )
      ) |>
    select(-group_id) |>
    tidyr::unnest(draws) |>
    rename(!!value_name := value)

  if (n_distinct(value_tbl$baseline_strata) == 1 &&
      identical(value_tbl$baseline_strata[[1]], "all")) {
    value_tbl <- value_tbl |>
      select(-baseline_strata)
    }

  if (summary) {
    grouping_vars <- c("time")

    if ("baseline_strata" %in% names(value_tbl)) {
      grouping_vars <- c(grouping_vars, "baseline_strata")
      }

    value_tbl <- value_tbl |>
      group_by(across(all_of(grouping_vars))) |>
      summarise(
        mean_value   = mean(.data[[value_name]]),
        median_value = median(.data[[value_name]]),
        lower_ci     = quantile(.data[[value_name]], 0.025),
        upper_ci     = quantile(.data[[value_name]], 0.975),
        .groups      = "drop"
        ) |>
      rename(
        !!mean_name := mean_value,
        !!median_name := median_value
        )
    }

  value_tbl
}


#' Extract baseline hazard from a brmsfit Cox model
#'
#' This function extracts the baseline hazard function from a brmsfit object
#' that uses the cox() family. It returns a tidy tibble with time points,
#' draw IDs, and corresponding baseline hazard values.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param summary Should the function return summary statistics (mean, median, CI)?
#'   If FALSE (default), returns all draws. If TRUE, returns summary statistics.
#' @param strata Optional character vector of baseline strata to return.
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
extract_baseline_hazard <- function(
    brmsfit_obj,
    summary = FALSE,
    strata = NULL
  ) {

  extract_brms_cox_baseline_tbl(
    brmsfit_obj = brmsfit_obj,
    basis_name  = "Zbhaz",
    value_name  = "baseline_hazard",
    mean_name   = "mean_hazard",
    median_name = "median_hazard",
    summary     = summary,
    strata      = strata
    )
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
#' @param strata Optional character vector of baseline strata to return.
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
extract_cumulative_hazard <- function(
    brmsfit_obj,
    summary = FALSE,
    strata = NULL
  ) {

  extract_brms_cox_baseline_tbl(
    brmsfit_obj = brmsfit_obj,
    basis_name  = "Zcbhaz",
    value_name  = "cumulative_hazard",
    mean_name   = "mean_cumhaz",
    median_name = "median_cumhaz",
    summary     = summary,
    strata      = strata
    )
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
#' @param strata Optional character vector of baseline strata to return.
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
extract_baseline_survival <- function(
    brmsfit_obj,
    summary = FALSE,
    strata = NULL
  ) {

  # Get cumulative baseline hazard
  cumhaz_tbl <- extract_cumulative_hazard(
    brmsfit_obj,
    summary = FALSE,
    strata = strata
    )

  # Calculate baseline survival: S_0(t) = exp(-H_0(t))
  survival_tbl <- cumhaz_tbl |>
    mutate(
      baseline_survival = exp(-cumulative_hazard)
      ) |>
    select(time, draw_id, baseline_survival)

  # Return summary statistics if requested
  if (summary) {
    survival_summary_tbl <- survival_tbl |>
      group_by(across(any_of(c("time", "baseline_strata")))) |>
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
  alpha_draws = 0.1,
  strata = NULL
  ) {

  # Get summary statistics
  hazard_summary_tbl <- extract_baseline_hazard(
    brmsfit_obj,
    summary = TRUE,
    strata = strata
    )

  # Start with base plot
  p <- ggplot2::ggplot(hazard_summary_tbl, ggplot2::aes(x = time))

  # Add individual draws if requested
  if (show_draws > 0) {
    hazard_draws_tbl <- extract_baseline_hazard(
      brmsfit_obj,
      summary = FALSE,
      strata = strata
      ) |>
      filter(draw_id %in% sample(unique(draw_id), min(show_draws, max(draw_id))))

    if ("baseline_strata" %in% names(hazard_draws_tbl)) {
      p <- p +
        ggplot2::geom_line(
          data  = hazard_draws_tbl,
          ggplot2::aes(y = baseline_hazard, group = interaction(baseline_strata, draw_id)),
          alpha = alpha_draws,
          color = "grey50"
          )
    } else {
      p <- p +
        ggplot2::geom_line(
          data  = hazard_draws_tbl,
          ggplot2::aes(y = baseline_hazard, group = draw_id),
          alpha = alpha_draws,
          color = "grey50"
          )
      }
    }

  # Add summary statistics
  p <- p +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
      alpha = 0.3,
      fill  = "steelblue"
      ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median_hazard),
      color = "steelblue",
      size  = 1
      ) +
    ggplot2::labs(
      title    = if ("baseline_strata" %in% names(hazard_summary_tbl)) "Baseline Hazard Function by Stratum" else "Baseline Hazard Function",
      subtitle = "Median with 95% credible interval",
      x        = "Time",
      y        = "Baseline Hazard"
      ) +
    ggplot2::theme_minimal()

  if ("baseline_strata" %in% names(hazard_summary_tbl)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(baseline_strata))
    }

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
  alpha_draws = 0.1,
  strata = NULL
  ) {

  # Get summary statistics
  survival_summary_tbl <- extract_baseline_survival(
      brmsfit_obj,
      summary = TRUE,
      strata = strata
      ) |>
    filter(
      time <= max_time
      )

  # Start with base plot
  p <- ggplot2::ggplot(survival_summary_tbl, ggplot2::aes(x = time))

  # Add individual draws if requested
  if (show_draws > 0) {
    survival_draws_tbl <- extract_baseline_survival(
        brmsfit_obj,
        summary = FALSE,
        strata = strata
        ) |>
      filter(
        draw_id %in% sample(unique(draw_id), min(show_draws, max(draw_id))),
        time <= max_time
        )

    if ("baseline_strata" %in% names(survival_draws_tbl)) {
      p <- p +
        ggplot2::geom_line(
          data  = survival_draws_tbl,
          ggplot2::aes(y = baseline_survival, group = interaction(baseline_strata, draw_id)),
          alpha = alpha_draws,
          color = "grey50"
          )
    } else {
      p <- p +
        ggplot2::geom_line(
          data  = survival_draws_tbl,
          ggplot2::aes(y = baseline_survival, group = draw_id),
          alpha = alpha_draws,
          color = "grey50"
          )
      }
    }

  # Add summary statistics
  p <- p +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
      alpha = 0.3,
      fill  = "darkgreen"
      ) +
    ggplot2::geom_line(
      ggplot2::aes(y = median_survival),
      color = "darkgreen",
      size  = 1
      ) +
    ggplot2::labs(
      title    = if ("baseline_strata" %in% names(survival_summary_tbl)) "Baseline Survival Function by Stratum" else "Baseline Survival Function",
      subtitle = "Median with 95% credible interval",
      x        = "Time",
      y        = "Baseline Survival Probability"
      ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal()

  if ("baseline_strata" %in% names(survival_summary_tbl)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(baseline_strata))
    }

  return(p)
}


#' Predict policy-level posterior survival probabilities from a brmsfit Cox model
#'
#' Computes individual-level posterior survival curves for policies in newdata
#' using a Cox PH model fitted with brms. The survival probability is:
#' S(t | x, stratum) = exp(-H_0(t; stratum) * exp(eta))
#' where H_0 is the cumulative baseline hazard (interpolated from training
#' time points) and eta = X * beta is the linear predictor.
#'
#' @param brmsfit_obj A brmsfit object fitted with family = cox()
#' @param newdata Tibble of policies to predict survival for
#' @param times Numeric vector of time points at which to evaluate survival
#' @param bhaz_var Character: column in newdata giving the baseline hazard
#'   stratum. Required for stratified models; auto-detected from formula if NULL.
#' @param ndraws Integer: number of posterior draws to use. If NULL, uses all.
#' @param policy_id_col Character: column name in newdata for policy IDs.
#'   If NULL, row indices are used.
#'
#' @return A tibble with columns policy_id, draw_id, interp_time, post_surv_prob.
#'   Pass to group_nest(policy_id, draw_id) to get the nested format expected
#'   by extract_survival_time().
#'
predict_policy_survivals_brms <- function(
    brmsfit_obj,
    newdata,
    times,
    bhaz_var      = NULL,
    ndraws        = NULL,
    policy_id_col = NULL
  ) {

  validate_brms_cox_model(brmsfit_obj)

  strata_tbl    <- extract_baseline_strata(brmsfit_obj)
  is_stratified <- nrow(strata_tbl) > 1L

  if (is_stratified && is.null(bhaz_var)) {
    formula_str <- deparse(formula(brmsfit_obj)$formula)
    m <- regexpr("bhaz\\(gr\\s*=\\s*(\\w+)", formula_str)
    if (m == -1L) {
      stop("Cannot auto-detect bhaz variable. Pass bhaz_var explicitly.")
      }
    bhaz_var <- sub("bhaz\\(gr\\s*=\\s*", "", regmatches(formula_str, m))
  }

  if (is_stratified) {
    strata_map     <- setNames(strata_tbl$group_id, strata_tbl$baseline_strata)
    new_stratum_id <- strata_map[as.character(newdata[[bhaz_var]])]
    if (anyNA(new_stratum_id)) {
      unknown <- unique(as.character(newdata[[bhaz_var]])[is.na(new_stratum_id)])
      stop(glue::glue("Unknown stratum values in newdata: {paste(unknown, collapse = ', ')}"))
    }
  } else {
    new_stratum_id <- rep(1L, nrow(newdata))
  }

  standata_fit <- make_standata(
    formula(brmsfit_obj),
    data   = brmsfit_obj$data,
    family = family(brmsfit_obj)
  )

  Zcbhaz           <- standata_fit$Zcbhaz
  resp_var         <- all.vars(formula(brmsfit_obj)$formula[[2]])[1]
  train_times      <- brmsfit_obj$data[[resp_var]]
  train_stratum_id <- if ("Jgrbhaz" %in% names(standata_fit)) {
    standata_fit$Jgrbhaz
  } else {
    rep(1L, nrow(Zcbhaz))
  }

  sbhaz_draws_lst <- extract_sbhaz_draws(brmsfit_obj)

  strata_cumhaz_lst <- purrr::imap(
    setNames(seq_len(nrow(strata_tbl)), strata_tbl$baseline_strata),
    ~{
      g_idx   <- .x
      g_label <- .y
      g_id    <- strata_tbl$group_id[g_idx]

      obs_idx <- which(train_stratum_id == g_id)
      times_g <- train_times[obs_idx]

      sbhaz_key <- if (g_label %in% names(sbhaz_draws_lst)) {
        g_label
      } else if (as.character(g_id) %in% names(sbhaz_draws_lst)) {
        as.character(g_id)
      } else {
        names(sbhaz_draws_lst)[g_idx]
      }

      sbhaz_m  <- sbhaz_draws_lst[[sbhaz_key]]
      basis_g  <- Zcbhaz[obs_idx, , drop = FALSE]
      cumhaz_m <- basis_g %*% t(sbhaz_m)

      sort_idx <- order(times_g)
      list(
        times  = times_g[sort_idx],
        cumhaz = cumhaz_m[sort_idx, , drop = FALSE]
      )
    }
  )

  rhs_expr    <- formula(brmsfit_obj)$formula[[3]]
  rhs_formula <- as.formula(paste("~", deparse(rhs_expr)))
  X_new       <- model.matrix(rhs_formula, data = newdata)[, -1L, drop = FALSE]

  x_col_names <- colnames(X_new)
  post_df     <- as_draws_df(brmsfit_obj)
  b_cols_brms <- paste0("b_", x_col_names)
  b_matrix    <- as.matrix(post_df[, b_cols_brms])

  n_total_draws <- nrow(b_matrix)
  draw_subset   <- if (!is.null(ndraws) && ndraws < n_total_draws) {
    sort(sample(n_total_draws, ndraws))
  } else {
    seq_len(n_total_draws)
  }
  n_draws <- length(draw_subset)

  b_matrix_sub      <- b_matrix[draw_subset, , drop = FALSE]
  strata_cumhaz_lst <- purrr::map(strata_cumhaz_lst, ~{
    .x$cumhaz <- .x$cumhaz[, draw_subset, drop = FALSE]
    .x
  })

  eta_matrix <- X_new %*% t(b_matrix_sub)

  policy_ids <- if (!is.null(policy_id_col)) newdata[[policy_id_col]] else seq_len(nrow(newdata))
  n_times    <- length(times)

  result_rows <- vector("list", n_draws)

  for (di in seq_len(n_draws)) {
    draw_rows <- vector("list", nrow(strata_tbl))

    for (g_idx in seq_len(nrow(strata_tbl))) {
      g_id    <- strata_tbl$group_id[g_idx]
      g_label <- strata_tbl$baseline_strata[g_idx]

      pol_idx <- which(new_stratum_id == g_id)
      if (length(pol_idx) == 0L) next

      cumhaz_g    <- strata_cumhaz_lst[[g_label]]
      H0_at_times <- approx(
        cumhaz_g$times, cumhaz_g$cumhaz[, di],
        xout = times,   rule = 2
      )$y

      eta_d    <- eta_matrix[pol_idx, di]
      surv_mat <- exp(-outer(H0_at_times, exp(eta_d)))

      draw_rows[[g_idx]] <- tibble::tibble(
        policy_id      = rep(policy_ids[pol_idx], each = n_times),
        draw_id        = di,
        interp_time    = rep(times, times = length(pol_idx)),
        post_surv_prob = as.vector(surv_mat)
      )
    }

    result_rows[[di]] <- dplyr::bind_rows(purrr::compact(draw_rows))
    if (di %% 200L == 0L) message(glue::glue("  draw {di}/{n_draws}"))
  }

  dplyr::bind_rows(result_rows)
}
