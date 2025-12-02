# Agent Guidelines for Bayesian Survival Analysis Project

This document provides guidance for AI assistants (GitHub Copilot, etc.) working with this codebase.

## Project Overview

This project performs Bayesian survival analysis on life insurance policy data, focusing on modeling policy lapses and customer lifetime behavior. The analysis uses R, Stan (via brms), and various survival analysis packages.

**Primary Goal**: Model and predict insurance policy lapse behavior using classical and Bayesian survival analysis techniques.

## Project Structure

```
bayesian_survival_analysis/
├── data/                           # Parquet data files
│   └── lifeins_policybook_inoutforce.parquet
├── stan_code/                      # Stan model definitions
│   └── brm_cox.stan
├── stan_model/                     # Compiled Stan models
├── build/                          # Docker build configurations
│   ├── Dockerfile
│   ├── docker_install_*.R
│   └── Renviron.site
├── tmp/                            # Temporary files and workflow configs
├── lib_*.R                         # Shared library functions
├── *.qmd                          # Quarto analysis notebooks
├── Justfile                       # Task automation (just command runner)
└── README.md                      # Project documentation
```

## Key Files

### Analysis Notebooks (Quarto .qmd)
- **`exploration_lifebook_data.qmd`**: Exploratory data analysis, data quality checks, dataset preparation (includes chunk timing)
- **`classic_survival_models.qmd`**: Classical survival models (Kaplan-Meier, Cox-PH)
- **`initial_bayesian_survival.qmd`**: Bayesian survival models (lapse1/2/3) using Stan/rstanarm with small dataset (includes manual caching and chunk timing)
- **`bayesian_survival_filtered_data.qmd`**: Same structure as initial but with filtered small dataset (prem_ape < 15k, start >= 2005) and filterlapse1/2/3 prefixes (includes manual caching and chunk timing)
- **`conditional_survival_prediction.qmd`**: Methods for calculating conditional survival probabilities

### Library Files
- **`lib_survival_modelling.R`**: Survival analysis helper functions (including `determine_policy_status()`)
- **`lib_brms_hazard.R`**: brms Cox model baseline hazard extraction and visualization functions
- **`lib_stan_diagnostics.R`**: Stan/rstanarm MCMC diagnostics (convergence and mixing plots)
- **`lib_utils.R`**: General utility functions (conflict resolution, `write_parquet_compressed()`, etc.)
- **`policy_status_functions.R`**: Policy-specific business logic

### Build & Infrastructure
- **`Justfile`**: Task runner for Docker operations, rendering, etc.
- **`Dockerfile`**: R environment with RStudio Server for reproducibility
- **`build/`**: Docker build scripts and R package installation

## Coding Conventions

### R Code Style
1. **Use tidyverse conventions**: pipes (`|>`), tibbles, functional programming
2. **Naming conventions**:
   - Snake case for variables: `policy_lifetime`, `sum_assured`
   - Suffix tibbles with `_tbl`: `data_tbl`, `model_data_tbl`, `summary_tbl`
   - Suffix plots with `_plot`: `survival_plot`, `hazard_plot`
   - Suffix models with model type: `lapse1_km`, `lapse2_coxph`
   - **Shared vs Model-Specific objects**:
     - **NO prefix** for objects shared across all models: `model_training_tbl`, `inforce_tbl`, `monthly_tbl`, `actual_monthly_tbl`
     - **Use `lapseN_` prefix** for model-specific objects: `lapse1_coxph_stansurv`, `lapse2_post_draws_lst`, `lapse3_sim_cashflow_tbl`
     - This convention makes it immediately clear which objects are reused vs. model-specific
3. **Function naming**: Verb-noun pattern when possible
4. **Namespace conflicts**: Always resolve using `conflicted` package (see `lib_utils.R`)
5. **Avoid base R shortcuts**: Use tidyverse pipelines instead of `$` accessor and `sum(df$col)`
   - ❌ Bad: `sum(data$column == value)` or `mean(data$column)`
   - ✅ Good: `data |> filter(column == value) |> nrow()` or `data |> pull(column) |> mean()`
6. **Use `summarise()` for aggregations**: Group multiple statistics in a single `summarise()` block
7. **Compute new columns with `mutate()`**: Create derived variables within the pipeline
8. **Parentheses indentation**: Function arguments should be indented 2 spaces from the line with the opening parenthesis
   - ❌ Bad (closing `)` at wrong indentation level):
     ```r
     data_tbl |>
       select(
         policy_id, lifetime
       )
     ```
   - ✅ Good (closing `)` indented 2 spaces from `select(` line):
     ```r
     data_tbl |>
       select(
         policy_id, lifetime
         )
     ```
   - The closing `)` should be indented 2 spaces from the line containing the opening `(`
   - For ggplot: Each layer's closing `)` follows the same rule
   - Example with proper alignment:
     ```r
     ggplot(data_tbl) +
       geom_point(
         aes(x = var1, y = var2, color = group),
         alpha = 0.5,
         size  = 2
         ) +
       labs(
         title = "Plot Title",
         x     = "X Label",
         y     = "Y Label"
         )
     ```

### Quarto Notebooks
1. **Chunk options**:
   ```r
   #| echo: false     # Hide code by default
   #| message: false  # Suppress messages
   #| warning: false  # Suppress warnings
   ```
2. **Use descriptive chunk labels**: `load_policy_data`, `fit_coxph1_model`
3. **Add narrative text** before and after code chunks explaining what's being done
4. **Figures**: Set appropriate `fig-width` and `fig-height` in chunk options at the start of the notebook
5. **Output formatting**: Use `message()` for informational output, `write_lines()` for formatted text blocks
6. **Code organization for multi-model notebooks**:
   - Create shared data objects in a dedicated section after data loading
   - Use clear section headers like "Create Shared Data Objects"
   - Front-load all objects that will be reused across models (monthly dates, subsets, lookups)
   - This reduces duplication and makes dependencies explicit
7. **Chunk timing**: Add automatic timing using knitr hooks:
   ```r
   chunk_times <- list()
   
   knitr::knit_hooks$set(
     time_it = function(before, options, envir) {
       if (before) {
         chunk_times[[options$label]] <<- list(start = Sys.time())
       } else {
         chunk_times[[options$label]]$end <<- Sys.time()
         chunk_times[[options$label]]$elapsed <<-
           chunk_times[[options$label]]$end |>
           difftime(chunk_times[[options$label]]$start) |>
           as.numeric()
       }
     }
   )
   
   knitr::opts_chunk$set(time_it = TRUE)
   ```
   - Add timing summary section before R Environment to display and save results
   - Saves timing data to `chunk_timings/` directory as parquet files

### Data Persistence
1. **File formats**:
   - Use **parquet** for tibbles: `object_tbl |> write_parquet_compressed("path/to/file.parquet")`
   - Use **qs** for complex nested structures (lists, model objects): `object_lst |> qsave("path/to/file.qs")`
   - Parquet provides better compression, portability, and performance for tabular data
   - qs package is 2-10x faster than RDS with better compression
2. **Always use pipe notation**: `object |> write_parquet_compressed(path)` not `write_parquet(object, path)`
3. **Parquet compression**: Use `write_parquet_compressed()` wrapper (from `lib_utils.R`) for zstd level 3 compression
4. **qs settings**: Use `preset = "balanced"` for good speed/compression tradeoff
5. **Save before remove**: When clearing large objects, save them first for reproducibility
   ```r
   # Save before removing
   large_tbl |> write_parquet_compressed(glue("{output_dir}/large_tbl.parquet"))
   rm(large_tbl)
   ```

### Stan/brms Models
1. **Model naming**: Use descriptive names indicating model type and iteration
2. **Always set seed**: Use `seed = 4000` (or similar) for reproducibility
3. **Document priors**: Comment on prior choices in text
4. **Save fitted models**: Use `file` argument in `brm()` to automatically cache fitted models
5. **cmdstanr backend**: Use `backend = "cmdstanr"` for better performance
6. **Model caching**: 
   - Use `file = "fitted_models/model_name"` to save/load fitted models automatically
   - Use `output_dir = "stan_output"` to control where CSV samples are stored
   - Use `output_basename = "model_name"` to give CSV files readable names (not UUIDs)
   - Example:
     ```r
     brm(
       formula,
       data = data_tbl,
       family = cox(),
       backend = "cmdstanr",
       file = "fitted_models/lapse1_brmsfit",
       output_dir = "stan_output",
       output_basename = "lapse1_brmsfit"
     )
     ```
7. **Manual caching for large objects**: For posterior draws and derived objects, use manual caching:
   ```r
   cache_file <- "stan_model/lapse1_post_draws_lst.qs"
   if (file_exists(cache_file)) {
     lapse1_post_draws_lst <- qread(cache_file)
   } else {
     lapse1_post_draws_lst <- posterior_survfit(...)
     lapse1_post_draws_lst |> qsave(cache_file, preset = "balanced")
   }
   ```
8. **Stan output visibility**: Set `daemon: false` in YAML execute options and `refresh = 200` in `stan_surv()`

## Domain-Specific Context

### Insurance Policy Data
- **Policy Status**: `inforce` (active) or `lapsed` (terminated)
- **Event Variable**: `lapsed` (1 = event occurred, 0 = censored)
- **Time Variable**: `policy_lifetime` (in weeks)
- **Key Predictors**:
  - Demographics: `gender_life1`, `dob_life1`, `smoker_life1`
  - Financial: `sum_assured`, `prem_ape`, `prem_sa_ratio`
  - Clustering: `cluster_id`

### Important: Age Calculations
- **DO NOT use `age_life1`** from the raw data - it's static and doesn't reflect time passage
- **ALWAYS calculate age from `dob_life1`** using `time_length(difftime())`:
  - `age_at_refdate`: Age at reference date (rollback/current date)
  - `age_at_statusdate`: Age when policy changed status
  - `age_at_observation`: Age at time of observation
- See `determine_policy_status()` in `lib_survival_modelling.R` for implementation

### Survival Analysis Concepts
- **Censoring**: In-force policies are right-censored (event hasn't occurred yet)
- **Time origin**: `policy_startdate`
- **Time scale**: Weeks from policy start
- **Event**: Policy lapse/termination

## Common Tasks

### Adding New Analysis
1. Create a new `.qmd` file with descriptive name
2. Use existing notebooks as templates for YAML header
3. Source library files: `source("lib_utils.R")`
4. Resolve conflicts early in the notebook
5. Set theme: `theme_set(theme_cowplot())`
6. Load data from `data/` directory using `arrow::read_parquet()`
7. Use `write_lines()` for text output

### Model Diagnostics
When adding model diagnostics, include:
- **For Cox-PH models**: 
  - Proportional hazards assumption (`cox.zph()`)
  - Residual plots (martingale, deviance, dfbeta)
  - Concordance statistics
  - Forest plots of hazard ratios
- **For Bayesian models**:
  - Posterior predictive checks
  - Trace plots and convergence diagnostics
  - Prior sensitivity analysis
  - LOO-CV for model comparison

### Visualization Guidelines
1. Use `ggplot2` + `cowplot` theme
2. For survival curves: Use `survminer::ggsurvplot()` or `ggsurvfit`
3. Add proper labels with `labs()`: title, subtitle, x, y, caption
4. Use `scales::label_comma()` and `scales::label_percent()` for axes
5. Color palettes: `scale_*_brewer()` with "Set1", "Set2", etc.
6. Multi-panel plots: Use `cowplot::plot_grid()`

### Data Transformations
1. **Time calculations**: Use `difftime()` with `units = "weeks"`
2. **Age calculations**: Use `time_length(difftime(...), unit = "years")`
3. **Derived variables**: Document creation in "Create Derived Variables" section
4. **Missing data**: Check and visualize before analysis
5. **Categorical encoding**: Use factors with meaningful levels

### Data Quality Checks
When comparing datasets (e.g., rollback vs current):
1. Create a `comparison_tbl` joining both datasets
2. Verify policy status consistency (no impossible transitions)
3. Check lifetime changes (lapsed = constant, inforce = increases)
4. Verify age calculations align with time passage
5. Confirm static variables remain unchanged
6. Use `write_lines()` for reporting check results

### Dataset Variants
The project maintains multiple dataset variants for different analyses:
1. **Size variants** (created in `exploration_lifebook_data.qmd`):
   - `model_training_small_tbl.parquet` / `model_rollforward_small_tbl.parquet`: Base datasets
   - `model_training_large_tbl.parquet` / `model_rollforward_large_tbl.parquet`: Larger sample
   
2. **Filtered variants** (created in `exploration_lifebook_data.qmd`):
   - `model_training_filtered_large_tbl.parquet` / `model_rollforward_filtered_large_tbl.parquet`
   - `model_training_filtered_small_tbl.parquet` / `model_rollforward_filtered_small_tbl.parquet`
   - Filters: `prem_ape < 15000`, `policy_startdate >= 2005-01-01`

3. **Usage pattern**:
   - `initial_bayesian_survival.qmd`: Uses small datasets with lapse1/2/3 prefixes
   - `bayesian_survival_filtered_data.qmd`: Uses filtered small datasets with filterlapse1/2/3 prefixes
   - Use `data_dir` variable for configurable paths: `data_dir <- "data"`

4. **Naming convention**:
   - Training data: Used for model fitting
   - Rollforward data: Used for predictions and validation
   - Prefix model objects to match dataset: `lapse1_*` for small, `filterlapse1_*` for filtered

## Docker & Environment

### Running the Container
```bash
just docker-run
```

This starts RStudio Server on port 8787 with:
- User namespace mapping (`--userns=keep-id`) for proper file ownership
- Volume mapping to workspace with SELinux support (`:z` flag)
- RUNROOTLESS=false to allow RStudio Server to function properly

### Common Issues
- **SELinux contexts**: Volume uses `:z` flag for proper SELinux labeling
- **Package installation**: Add to `build/docker_install_user_rpkgs.R`
- **System dependencies**: Add to `build/docker_install_sys_rpkgs.R`

## Dependencies

### Core R Packages
- **tidyverse**: Data manipulation and visualization
- **survival**: Classical survival analysis
- **survminer**: Enhanced survival visualizations
- **ggsurvfit**: Modern survival curve plotting
- **rstanarm**: Bayesian regression models using Stan (used for `stan_surv()`)
- **brms**: Bayesian regression models using Stan
- **tidybayes**: Tidy data extraction from Bayesian models
- **arrow**: Reading/writing parquet files with multi-threading support
- **qs**: Fast serialization for R objects (2-10x faster than RDS)
- **cowplot**: Publication-quality plots and multi-panel layouts
- **bayesplot**: MCMC diagnostics and visualization
- **DataExplorer**: Automated exploratory data analysis
- **glue**: String interpolation for messages
- **fs**: Cross-platform file system operations

### Analysis-Specific
- **muhaz**: Hazard function estimation
- **broom**: Tidy model outputs
- **gtsummary**: Publication-ready tables
- **conflicted**: Namespace conflict management
- **lubridate**: Date/time operations

### Bayesian Analysis Tools

#### brms Cox Models (lib_brms_hazard.R)
The `lib_brms_hazard.R` library provides functions for working with brmsfit Cox models:
- **`extract_baseline_hazard()`**: Extract baseline hazard as tidy tibble with time, draw_id, and hazard values
- **`extract_cumulative_hazard()`**: Extract cumulative baseline hazard
- **`extract_baseline_survival()`**: Extract baseline survival function
- **`plot_baseline_hazard()`**: Visualize baseline hazard with uncertainty
- **`plot_baseline_survival()`**: Visualize baseline survival with uncertainty

All extraction functions support `summary = TRUE` to return summary statistics (mean, median, credible intervals) instead of individual draws.

**Implementation Details**:
- Uses the stored M-spline basis matrix from `brmsfit_obj$basis$dpars$mu$bhaz$basis_matrix`
- Extracts time points directly from `brmsfit_obj$data[[time_var]]` (the actual observation times)
- Computes baseline hazard as: `basis_matrix %*% sbhaz_coefs` (no exponentiation needed)
- `sbhaz` represents actual hazard coefficients, not log hazard
- Cumulative hazard computed via `cumsum()` for each posterior draw
- No dependency on `splines2` package - uses brms' stored basis directly

**Note**: These functions extract from the actual fitted model components, ensuring consistency with brms' internal calculations. Avoid reconstructing spline bases manually.

#### rstanarm Cox Models (lib_survival_modelling.R)
The `lib_survival_modelling.R` library provides functions for working with rstanarm Cox models:
- **`extract_stansurv_baseline_hazard(stanfit, summary=FALSE, n=1000)`**: Extract baseline hazard from `stan_surv()` fitted objects
  - Returns tidy tibble with time, draw_id, and baseline_hazard columns
  - Supports `summary=TRUE` to return mean, median, and 95% CI instead of individual draws
  - Time points generated uniformly from min to max observed times
- **`compare_baseline_hazards(stansurv_fit, coxph_fit, max_time=Inf, plot_type="cumulative_hazard", n=1000, prob=0.95)`**: Compare Bayesian and frequentist baseline hazards or survival curves
  - Overlays `stan_surv()` and `coxph()` estimates
  - Supports three plot types: `"cumulative_hazard"` (default), `"instantaneous_hazard"`, or `"survival"`
  - Returns list with plot and comparison data tibble
  - Use `max_time` to restrict time range (e.g., `max_time=156` for 3 years in weeks)

**Implementation Details (rstanarm)**:
- M-spline basis stored in `stanfit$basehaz$basis` object
- M-spline coefficients stored as `m-splines-coef1`, `m-splines-coef2`, etc. in posterior draws
- Basis evaluated at time points using `predict(basehaz$basis, times)`
- Baseline hazard computed as: `exp(intercept) * (aux %*% t(basis_mat))` (intercept optional)
- For comparison: interpolates `coxph()` estimates to match `stan_surv()` time points using `approx()`
- Cumulative hazard from `basehaz()`, instantaneous from numeric differentiation

**Usage Pattern**:
```r
# After fitting models
lapse1_coxph_stansurv <- stan_surv(...)
lapse1_coxph <- coxph(...)

# Extract baseline hazard
baseline_hazard_tbl <- extract_stansurv_baseline_hazard(lapse1_coxph_stansurv)

# Compare cumulative hazards (restricted to 3 years)
comparison_lst <- compare_baseline_hazards(
  lapse1_coxph_stansurv,
  lapse1_coxph,
  max_time = 156,  # 3 years in weeks
  plot_type = "cumulative_hazard"
)
comparison_lst$plot

# Compare survival curves
survival_comparison_lst <- compare_baseline_hazards(
  lapse1_coxph_stansurv,
  lapse1_coxph,
  max_time = 156,
  plot_type = "survival"
)
survival_comparison_lst$plot
```

### Stan Diagnostics Tools
The `lib_stan_diagnostics.R` library provides functions for MCMC diagnostics on Stan/rstanarm models:
- **`plot_stan_convergence(stanfit_obj, max_rhat)`**: 3-panel convergence diagnostics
  - Rhat values plot (convergence indicator)
  - Effective sample size ratio plot
  - Text summary of diagnostics
  - Returns cowplot grid with 14x5 inch dimensions

- **`plot_stan_mixing(stanfit_obj, pars, n_draws_overlay, max_lag)`**: Mixing and autocorrelation diagnostics
  - Auto-selects 6 parameters with lowest n_eff ratios when `pars = NULL`
  - Displays comma-separated list of selected parameters
  - Trace plots for chains convergence
  - Density overlays for posterior distribution
  - Autocorrelation plots for chain mixing
  - Returns cowplot grid with 14x10 inch dimensions

**Usage Pattern**:
```r
# After fitting Stan model
lapse1_coxph_stansurv <- stan_surv(...)

# Convergence diagnostics
plot_stan_convergence(lapse1_coxph_stansurv)

# Mixing diagnostics (auto-selects worst 6 parameters)
plot_stan_mixing(lapse1_coxph_stansurv)
```

**Implementation Notes**:
- Functions use bare package names (no `::` prefixes)
- Auto-excludes baseline hazard parameters (sbhaz)
- Smart parameter selection based on n_eff ratios
- Integrates bayesplot functions for standard MCMC diagnostics

### Performance Optimization
- **Arrow threading**: Set `arrow::set_cpu_count()` and `arrow::set_io_thread_count()` for parallel I/O
- **Parquet compression**: Use `write_parquet_compressed()` wrapper for zstd level 3 compression (see `lib_utils.R`)
- **qs package**: Use `qsave()`/`qread()` with `preset = "balanced"` for R objects (2-10x faster than RDS)
- **Manual caching**: Use `file_exists()` pattern for objects >2GB to avoid R's long vector limitations

## Best Practices

### When Modifying Code
1. **Read existing code first**: Understand patterns and conventions
2. **Check for similar implementations**: Look at other `.qmd` files for examples
3. **Test incrementally**: Render sections of Quarto docs as you go
4. **Document thoroughly**: Add text explanations in Quarto notebooks
5. **Preserve reproducibility**: Don't change random seeds without good reason

### When Adding Features
1. **Consider performance**: Large datasets may need sampling
2. **Handle missing data**: Check and document missing value patterns
3. **Validate assumptions**: Test model assumptions explicitly
4. **Compare models**: Use AIC, BIC, concordance for comparison
5. **Visualize results**: Always include appropriate plots

### When Working with Multi-Model Notebooks
1. **Identify shared objects early**: Determine which objects are used across all models
2. **Create a dedicated section**: Place "Create Shared Data Objects" after data loading
3. **Clear naming conventions**: No prefix for shared objects, `modelN_` prefix for model-specific
4. **Front-load common computations**: Monthly dates, subsets, lookup tables, actual cashflows
5. **Document dependencies**: Use comments to explain which objects depend on others
6. **Example structure**:
   ```r
   # Load Data section
   model_training_tbl <- read_parquet(...)
   model_rollforward_tbl <- read_parquet(...)
   
   # Create Shared Data Objects section
   monthly_tbl <- tibble(...)
   inforce_tbl <- model_training_tbl |> filter(...)
   actual_monthly_tbl <- model_rollforward_tbl |> ...
   
   # Build Model 1 section
   lapse1_coxph <- stan_surv(..., data = model_training_tbl)
   lapse1_predictions <- predict(lapse1_coxph, newdata = inforce_tbl)
   
   # Build Model 2 section
   lapse2_coxph <- stan_surv(..., data = model_training_tbl)
   lapse2_predictions <- predict(lapse2_coxph, newdata = inforce_tbl)
   ```

### When Debugging
1. **Check data first**: Use `glimpse()`, `summary()`, `count()`
2. **Verify dimensions**: Ensure expected row counts after filters/joins
3. **Inspect intermediates**: Use `print()` or `write_lines()` liberally during development
4. **Check for NAs**: They can cause silent failures in survival models
5. **Review error messages**: Stan errors often indicate prior/data mismatches

### Output Formatting in Quarto
1. **Use `write_lines()` from readr (tidyverse)**: Works reliably regardless of chunk options
2. **Always specify `stdout()` as output**: `write_lines(text, stdout())` ensures proper rendering
3. **Pre-compute statistics using tidyverse**: Use `summarise()` to create summary tibbles
4. **Example patterns**:
   ```r
   # Simple output
   write_lines("Summary Statistics:", stdout())
   
   # Tidyverse approach with summarise()
   summary_tbl <- data_tbl |>
     summarise(
       count = n(),
       mean_val = mean(value, na.rm = TRUE),
       sd_val = sd(value, na.rm = TRUE)
     )
   
   write_lines(glue(
     "Summary Statistics:
       Count: {summary_tbl$count}
       Mean: {format(summary_tbl$mean_val, digits = 4)}
       SD: {format(summary_tbl$sd_val, digits = 4)}"
   ), stdout())
   
   # Using pull() for single values
   mean_val <- data_tbl |> pull(value) |> mean(na.rm = TRUE)
   write_lines(glue("Mean: {format(mean_val, digits = 4)}"), stdout())
   ```
5. **Note**: Always use `stdout()` as the second argument to `write_lines()` for Quarto output

## Justfile Usage

### Common Commands
- **`just docker-run`**: Start Docker container with RStudio Server
- **`just docker-build`**: Build Docker image
- **`just exploration_lifebook_data`**: Render exploration notebook
- **`just classic_survival_models`**: Render classical survival models notebook
- **`just conditional_survival_prediction`**: Render conditional survival notebook

### Note on Justfile Syntax
- Current version (1.40.0) has issues parsing periods in target names
- Use underscores instead: `exploration_lifebook_data` not `exploration_lifebook_data.html`
- Dependencies specified without file extensions

## Git Workflow

**Current Branch**: `build-init-survmodel`

### Typical Workflow
1. Make changes in feature branch
2. Test by rendering notebooks: `quarto render filename.qmd`
3. Commit with descriptive messages
4. Push when stable

### Commit Message Style
- Use present tense: "Add model diagnostics" not "Added model diagnostics"
- Be specific: "Add Cox-PH residual plots" not "Update analysis"
- Reference issues when applicable

## Additional Resources

### Survival Analysis
- **Kaplan-Meier**: Non-parametric survival curve estimation
- **Cox Proportional Hazards**: Semi-parametric regression model
- **Bayesian Survival**: Full posterior inference on survival parameters

### Quarto
- Official docs: https://quarto.org/docs/
- Execute code: `quarto render filename.qmd`
- Preview: `quarto preview filename.qmd`

### Stan/brms
- brms vignettes: https://paul-buerkner.github.io/brms/
- Stan docs: https://mc-stan.org/docs/
- Prior choice guide: https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations

## Tips for AI Assistants

1. **Context matters**: Always check existing code for patterns before suggesting new approaches
2. **Preserve style**: Match the existing coding style and conventions
3. **Be explicit**: When adding model code, include full diagnostics and interpretations
4. **Think about users**: Add helpful text explanations in Quarto documents
5. **Consider reproducibility**: Ensure all random processes are seeded
6. **Validate suggestions**: Cross-reference with package documentation
7. **Domain knowledge**: Remember this is insurance data with specific business context
8. **Performance**: Large Stan models can take hours; suggest sampling for development
9. **Documentation**: Update this file if you add new conventions or patterns
10. **Testing**: Suggest ways to validate models (out-of-sample, cross-validation, etc.)
11. **Age calculations**: Never use `age_life1` - always calculate from `dob_life1`
12. **Output formatting**: Use `write_lines(text, stdout())` in Quarto notebooks
13. **Tidyverse style**: Always suffix tibbles with `_tbl` and avoid `$` accessor in favor of `pull()`
14. **Parentheses indentation**: Indent function arguments to align with code above, not at enclosing level; closing `)` on own line

## Questions to Ask Before Making Changes

- Have I read the relevant existing code?
- Am I following the project's naming conventions?
- Do I understand the domain context (insurance policies, survival analysis)?
- Have I included appropriate documentation/narrative text?
- Will this work with the Docker environment?
- Are there similar implementations I can learn from?
- Have I considered computational cost (especially for Stan models)?
- Is this change consistent with the project's goals?
- Am I using `dob_life1` for age calculations (not `age_life1`)?
- Am I using `write_lines(text, stdout())` for text output in Quarto?
- Are all tibbles named with `_tbl` suffix?
- Am I using tidyverse pipelines instead of `$` accessor?
- Are function arguments indented to align with code above, with closing `)` on own line?

## Recent Updates & Known Issues

### Performance Optimization & File Formats (November 2025)
- **Switched from RDS to qs package**: 2-10x faster serialization for R objects
  - All model objects and lists now use `.qs` extension with `qsave()`/`qread()`
  - Use `preset = "balanced"` for good speed/compression tradeoff
- **Arrow optimization**: Configured multi-threaded I/O for parquet operations
  - `set_cpu_count()` and `set_io_thread_count()` set in notebook setup
  - Provides 2-4x speedup for large file operations
- **Parquet compression**: Created `write_parquet_compressed()` wrapper in `lib_utils.R`
  - Uses zstd compression level 3 by default
  - Reduces file sizes significantly without performance penalty
- **Pipe notation standardization**: All file writes use pipe notation
  - `object_tbl |> write_parquet_compressed(path)`
  - `object_lst |> qsave(path, preset = "balanced")`

### Chunk Timing Infrastructure (November 2025)
- **Automatic chunk timing**: Added knitr hooks to all main notebooks
  - Tracks start/end time for every chunk
  - Uses `difftime()` with multiline indentation and `|>` pipe operator
  - Saves timing data to `chunk_timings/` directory as parquet files
- **Timing summary sections**: All notebooks include comprehensive timing reports
  - Displays chunks sorted by execution time (slowest first)
  - Formats times as seconds, minutes, or hours based on duration
  - Shows total execution time for entire notebook
- **Notebooks with timing**: `initial_bayesian_survival.qmd`, `bayesian_survival_filtered_data.qmd`, `exploration_lifebook_data.qmd`

### Manual Caching Strategy (November 2025)
- **Implemented comprehensive caching**: 15 expensive chunks per modeling notebook
  - Stan models: `lapse1/2/3_coxph_stansurv.qs` (and `filterlapse1/2/3_*` variants)
  - Posterior draws: `lapse1/2/3_post_draws_lst.qs`
  - Survival estimates: `lapse1/2/3_post_survival_tbl.parquet`
  - Simulated lapse times: `lapse1/2/3_sim_lapse_times_tbl.parquet`
  - Cashflows: `lapse1/2/3_sim_cashflow_tbl.parquet`
- **Caching pattern**: `file_exists()` → `qread()`/`read_parquet()` → compute → `qsave()`/`write_parquet_compressed()`
- **Rationale**: Manual caching avoids R's "long vectors not supported" limitation with Quarto's built-in cache

### Stan Diagnostics Library (November 2025)
- **Created `lib_stan_diagnostics.R`**: Comprehensive MCMC diagnostic functions
  - `plot_stan_convergence()`: Rhat, ESS ratio, diagnostics summary (3-panel, 14x5 inches)
  - `plot_stan_mixing()`: Trace plots, density overlays, autocorrelation (2+1 panels, 14x10 inches)
- **Smart parameter selection**: `plot_stan_mixing()` auto-selects 6 parameters with lowest n_eff ratios
  - Displays comma-separated list of selected parameters with 80-character wrapping
  - Makes diagnostic plots more useful by focusing on problematic parameters
- **Code style**: Functions use bare package names (no `::` prefixes) for cleaner code

### Dataset Variants & Filtered Data (November 2025)
- **Multiple dataset variants**: Standard and filtered versions for different analysis needs
  - Small: Used in `initial_bayesian_survival.qmd` with `lapse1/2/3` prefixes
  - Filtered small: Used in `bayesian_survival_filtered_data.qmd` with `filterlapse1/2/3` prefixes
- **Filtered datasets**: Created filtered variants in `exploration_lifebook_data.qmd`
  - Filter criteria: `prem_ape < 15000`, `policy_startdate >= 2005-01-01`
  - Both training and rollforward versions for small and large sizes
  - Files: `model_*_filtered_{large|small}_tbl.parquet`
  - Focus on more recent policies with lower premiums for homogeneous analysis
- **Configurable paths**: Use `data_dir` variable to avoid hardcoded paths

### Visualization Enhancements (November 2025)
- **Added ribbon plots**: All three models now have both line plots and ribbon plots
  - Ribbon plots show 50% and 80% credible intervals for uncertainty visualization
  - Applied to both all-policies and recent-policies (2010+) views
- **Shared data organization**: Moved common data definitions to dedicated section
  - `actual_monthly_tbl` and `actual_monthly_recent_tbl` created once and reused
  - Reduces duplication and makes dependencies explicit

### Notebook Refactoring: Shared Objects and Naming (November 2025)
- Standardized naming convention: shared objects have NO prefix, model-specific objects use `lapseN_` prefix
- Reorganized `initial_bayesian_survival.qmd`: moved shared objects to dedicated section after data loading
- Shared objects: `model_training_tbl`, `model_rollforward_tbl`, `monthly_tbl`, `inforce_tbl`, `policy_cutdown_tbl`, `actual_monthly_tbl`, `cashflow_week_count`
- Model-specific objects: `lapse1_coxph_stansurv`, `lapse2_post_draws_lst`, `lapse3_sim_cashflow_tbl`, etc.
- All write operations use pipe notation with appropriate compression

### lib_brms_hazard.R Implementation (November 2025)
- Functions now use stored basis matrix from `brmsfit_obj$basis$dpars$mu$bhaz$basis_matrix`
- Time points extracted from actual data: `brmsfit_obj$data[[time_var]]`
- No longer reconstructs splines - uses brms' internal representations
- `sbhaz` coefficients represent actual hazard (not log hazard) - no exponentiation needed
- Removed dependency on `splines2` package
- Summary statistics computed using `summarise()` instead of `median_qi()` to avoid metadata warnings

### Docker/Podman Configuration (November 2025)
- Uses `--userns=keep-id` flag to map container user to host user (maintains file ownership)
- Requires `RUNROOTLESS=false` for RStudio Server to function properly
- Volume mount uses `:z` SELinux flag for proper file access permissions
- Configuration allows seamless file editing both inside and outside container

### Age Calculation (October 2025)
- Removed all references to `age_life1` from models
- Updated `determine_policy_status()` to calculate ages from `dob_life1`
- All notebooks now use time-dependent age calculations

### Justfile Workaround (October 2025)
- Just 1.40.0 has period-parsing issues in target names
- Current workaround: Use underscores in target names
- May be resolved in future Just versions

### Data Quality Checks (October 2025)
- Added comprehensive sanity checks in `exploration_lifebook_data.qmd`
- Validates consistency between rollback and current datasets
- Verifies policy status transitions, lifetime changes, and age calculations

### Output Formatting (October 2025)
- Use `write_lines(text, stdout())` from readr (tidyverse) for text output in Quarto notebooks
- Always explicitly specify `stdout()` as the output destination
- `write_lines()` works regardless of chunk options (`message: false`, etc.)
- Combine with `glue()` for formatted output with embedded variables

### Tidyverse Style Conventions (October 2025)
- All tibbles must have `_tbl` suffix: `data_tbl`, `summary_tbl`, `comparison_tbl`
- Avoid `$` accessor - use `pull()` instead: `data_tbl |> pull(column) |> mean()`
- Never use `sum(df$column == value)` - use `df |> filter(column == value) |> nrow()`
- Use `summarise()` for computing multiple statistics at once
- Create derived columns with `mutate()` within the pipeline
- Keep code style consistent with early sections of notebooks

### Parentheses Indentation (October 2025)
- Function arguments indented to align with code above, not at enclosing level
- Closing `)` on its own line at same indentation as opening function call
- Applied consistently across all function calls and ggplot layers
- See early sections of `exploration_lifebook_data.qmd` for examples

### Quarto Caching Strategy (November 2025)
- **Manual caching** for Stan models and large posterior objects (recommended approach)
- Use `file_exists()` + `read_rds()`/`write_rds()` pattern for explicit control
- Avoid Quarto's built-in `#| cache: true` for very large objects (>2GB) due to R's "long vectors not supported" limitation
- Posterior draws from `posterior_survfit()` should use manual caching to RDS files
- Downstream processing chunks (simulations, cashflows) can use Quarto caching with `#| cache: true`
- Manual caching provides: explicit invalidation control, persistence across renders, smaller cache files

### Stan Output Configuration (November 2025)
- Use `#| results: hide` (not `#| output: false`) for console-only Stan progress
- Set `refresh = 200` in `stan_surv()` for progress updates every 200 iterations
- Add `cat()` + `flush.console()` before/after Stan calls for Docker visibility
- Docker may buffer stderr (where Stan writes) - use unbuffered output when needed
- Stan sampling messages go to stderr, not stdout

### Output Functions in Quarto (November 2025)
- Prefer `message()` over `cat()` for informational output
- Use `message(glue("text: {variable}"))` for string interpolation
- `message()` writes to stderr (separable from results) and can be suppressed
- Still use `write_lines(text, stdout())` for formatted text blocks in output

### rstanarm Baseline Hazard Extraction (November 2025)
- **Created baseline hazard extraction functions** for `stan_surv()` fitted objects in `lib_survival_modelling.R`
  - `extract_stansurv_baseline_hazard()`: Extracts M-spline-based baseline hazard from rstanarm Cox models
  - `compare_baseline_hazards()`: Compares Bayesian (stan_surv) and frequentist (coxph) baseline hazards
- **Technical discoveries**:
  - M-spline coefficients stored as `m-splines-coef1` through `m-splines-coefN` (not `s-basehaz`)
  - Basis matrix accessed via `stanfit$basehaz$basis` and evaluated with `predict()`
  - Formula: `exp(intercept) * (aux %*% t(basis_mat))` where aux is (n_draws × n_coefs) and basis_mat is (n_coefs × n_times)
  - Matrix multiplication order critical: results in (n_draws × n_times) hazard matrix
  - Data arrangement: Use `rep(times, times=n_draws)` and `rep(1:n_draws, each=n_times)` for proper tibble structure
- **Validation approach**:
  - Compared output with `plot(stanfit, plotfun="basehaz")` internal implementation
  - Examined rstanarm source code from feature/survival branch to understand M-spline representation
  - Confirmed exact match using `all.equal()` between extracted and plotted values
- **Comparison methodology**:
  - Uses `approx()` to interpolate coxph estimates to stan_surv time points
  - Supports cumulative hazard, instantaneous hazard, and survival curve comparison
  - Survival curves computed as S(t) = exp(-H(t)) from cumulative hazards
  - Applies time restrictions via `max_time` parameter (e.g., 156 weeks = 3 years)

---

**Last Updated**: November 28, 2025  
**Maintainer**: Mick Cooney (mcooney@describedata.com)
