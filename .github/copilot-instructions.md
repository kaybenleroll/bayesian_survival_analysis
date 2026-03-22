# GitHub Copilot Instructions for Bayesian Survival Analysis Project

> **Primary Reference**: See `AGENTS.md` in the project root for comprehensive guidelines.

## Quick Reference for Code Generation

### R Code Style (CRITICAL)

1. **Always use tidyverse conventions**:
   - Use native pipe `|>` (not magrittr `%>%`)
   - Suffix all tibbles with `_tbl`: `data_tbl`, `model_data_tbl`, `summary_tbl`
   - Suffix plots with `_plot`: `survival_plot`, `hazard_plot`
   - Suffix models with type: `lapse1_km`, `lapse2_coxph`, `lapse3_brmsfit`
   - Use snake_case for all variables: `policy_lifetime`, `sum_assured`

2. **NEVER use base R shortcuts**:
   - ❌ BAD: `sum(data$column == value)` or `mean(data$column)`
   - ✅ GOOD: `data |> filter(column == value) |> nrow()` or `data |> pull(column) |> mean()`
   - Always use `pull()` instead of `$` accessor in pipelines

3. **Function argument indentation**:
   - Indent arguments 2 spaces from the line with opening `(`
   - Place closing `)` on its own line at same indentation as opening function
   - Example:
     ```r
     data_tbl |>
       select(
         policy_id, lifetime, status
         ) |>
       filter(
         status == "lapsed"
         )
     ```

### Domain-Specific Rules (CRITICAL)

1. **Age Calculations**:
   - ❌ NEVER use `age_life1` from raw data (it's static)
   - ✅ ALWAYS calculate from `dob_life1` using:
     ```r
     age_at_date <- time_length(difftime(reference_date, dob_life1), unit = "years")
     ```

2. **Insurance Policy Context**:
   - Status: `inforce` (active) or `lapsed` (terminated)
   - Event variable: `lapsed` (1 = event, 0 = censored)
   - Time variable: `policy_lifetime` (in weeks)
   - Time origin: `policy_startdate`

### Bayesian Survival Models with brms

**Always include these parameters**:
```r
brm(
  formula,
  data = data_tbl,
  family = cox(bhaz = list(df = 8)),  # Set baseline hazard df explicitly
  backend = "cmdstanr",               # Use cmdstanr backend
  chains = 4,
  cores = 4,
  seed = 4000,                        # Project standard seed
  file = "fitted_models/model_name",  # Auto-caching
  output_dir = "stan_output",         # Control CSV location
  output_basename = "model_name"      # Readable filenames
)
```

**Baseline hazard degrees of freedom**:
- Use `family = cox(bhaz = list(df = 8))` to set M-spline basis df
- Default is usually 6; increase to 10-15 for complex patterns
- Access stored basis at `brmsfit$basis$dpars$mu$bhaz$basis_matrix`

### Quarto Notebooks

1. **Text output** (use `write_lines()` from readr):
   ```r
   write_lines("Summary Statistics:", stdout())
   
   # With glue for formatting
   write_lines(glue(
     "Count: {summary_tbl$count}
      Mean: {format(summary_tbl$mean_val, digits = 4)}"
   ), stdout())
   ```

2. **Standard chunk options**:
   ```r
   #| echo: false
   #| message: false
   #| warning: false
   #| fig-width: 10
   #| fig-height: 6
   ```

3. **Always add narrative text** before and after code chunks explaining purpose and results

### Visualization Standards

```r
# Use cowplot theme
theme_set(theme_cowplot())

# Proper labeling
ggplot(data_tbl) +
  geom_point(aes(x = var1, y = var2)) +
  labs(
    title = "Descriptive Title",
    x = "X Label",
    y = "Y Label",
    caption = "Source: Policy data"
    ) +
  scale_y_continuous(labels = scales::label_comma())
```

### Baseline Hazard Extraction (brms Cox models)

Use functions from `lib_brms_hazard.R`:

```r
source("lib_brms_hazard.R")

# Extract baseline hazard
baseline_hazard_tbl <- extract_baseline_hazard(
  brmsfit_obj,
  summary = TRUE  # Get mean, median, credible intervals
)

# Plot
plot_baseline_hazard(brmsfit_obj)
plot_baseline_survival(brmsfit_obj)
```

**Implementation notes**:
- Uses stored M-spline basis from `brmsfit$basis$dpars$mu$bhaz$basis_matrix`
- Time points from `brmsfit$data[[time_var]]`
- `sbhaz` coefficients are actual hazard (not log) - no exponentiation needed
- Computation: `baseline_hazard = basis_matrix %*% sbhaz_coefs`

### Docker Environment

When suggesting Docker commands:
```bash
# Start RStudio Server (port 8787)
just docker-run

# Build image
just docker-build
```

Configuration uses:
- `--userns=keep-id` for file ownership mapping
- `RUNROOTLESS=false` for RStudio Server
- Volume mount with `:z` SELinux flag

### Model Diagnostics

**For Cox-PH models**, always include:
- Proportional hazards test: `cox.zph()`
- Residual plots (martingale, deviance, dfbeta)
- Concordance statistics
- Forest plots of hazard ratios

**For Bayesian models**, include:
- Posterior predictive checks
- Trace plots: `plot(brmsfit)`
- Convergence diagnostics: `summary(brmsfit)`
- LOO-CV for comparison: `loo(model1, model2)`

### Common Patterns

**Data quality checks**:
```r
# Check for NAs
data_tbl |>
  summarise(
    across(everything(), ~sum(is.na(.)))
    )

# Verify policy status
data_tbl |>
  count(status, lapsed)
```

**Survival data preparation**:
```r
# Calculate derived variables
model_data_tbl <- data_tbl |>
  mutate(
    age_at_start = time_length(difftime(policy_startdate, dob_life1), "years"),
    policy_lifetime = as.numeric(difftime(statusdate, policy_startdate, units = "weeks")),
    lapsed = if_else(status == "lapsed", 1, 0)
    )
```

## Key Files

- `AGENTS.md`: Complete project documentation (READ THIS FIRST)
- `lib_brms_hazard.R`: Baseline hazard extraction functions
- `lib_survival_modelling.R`: Survival analysis helpers (including `determine_policy_status()`)
- `lib_utils.R`: Conflict resolution and utilities
- `Justfile`: Task automation (Docker, rendering)
- `exploration_lifebook_data.qmd`: Data exploration patterns
- `classic_survival_models.qmd`: Classical survival analysis examples
- `initial_bayesian_survival.qmd`: Bayesian model examples

## What NOT to Do

1. ❌ Don't use `age_life1` column - always calculate from `dob_life1`
2. ❌ Don't use `$` accessor - use `pull()` in pipelines
3. ❌ Don't use `cat()` or `print()` in Quarto - use `write_lines(text, stdout())`
4. ❌ Don't reconstruct M-spline basis - use stored `brmsfit$basis$dpars$mu$bhaz$basis_matrix`
5. ❌ Don't forget to set `seed = 4000` for reproducibility
6. ❌ Don't skip model caching - always use `file` argument in `brm()`
7. ❌ Don't use `summarise()` without grouping when you need row-wise operations
8. ❌ Don't forget narrative text in Quarto notebooks

## When in Doubt

1. Check existing `.qmd` files for patterns
2. Reference `AGENTS.md` for detailed guidelines
3. Use `lib_*` functions instead of reimplementing
4. Follow tidyverse style guide
5. Test code incrementally in R console before adding to notebooks
6. Always validate assumptions (check for NAs, verify data types)

---

**For complete documentation, see `AGENTS.md` in the project root.**
