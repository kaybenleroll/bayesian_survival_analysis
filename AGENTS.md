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
- **`exploration_lifebook_data.qmd`**: Exploratory data analysis, data quality checks, dataset preparation
- **`classic_survival_models.qmd`**: Classical survival models (Kaplan-Meier, Cox-PH)
- **`initial_bayesian_survival.qmd`**: Bayesian survival models using Stan/brms
- **`conditional_survival_prediction.qmd`**: Methods for calculating conditional survival probabilities

### Library Files
- **`lib_survival_modelling.R`**: Survival analysis helper functions (including `determine_policy_status()`)
- **`lib_utils.R`**: General utility functions (conflict resolution, etc.)
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
5. **Output formatting**: Use `write_lines()` from readr for text output to ensure clean rendering

### Stan/brms Models
1. **Model naming**: Use descriptive names indicating model type and iteration
2. **Always set seed**: Use `stanfit_seed <- 4000` for reproducibility
3. **Document priors**: Comment on prior choices in text
4. **Save fitted models**: Use `saveRDS()` for large models to avoid refitting

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

## Docker & Environment

### Running the Container
```bash
just docker-run
```

This starts RStudio Server on port 8787 with volume mapping to workspace.

### Common Issues
- **Permission issues**: Container runs as `rstudio` user; use `:z` flag for SELinux
- **Package installation**: Add to `build/docker_install_user_rpkgs.R`
- **System dependencies**: Add to `build/docker_install_sys_rpkgs.R`

## Dependencies

### Core R Packages
- **tidyverse**: Data manipulation and visualization
- **survival**: Classical survival analysis
- **survminer**: Enhanced survival visualizations
- **ggsurvfit**: Modern survival curve plotting
- **brms**: Bayesian regression models using Stan
- **arrow**: Reading parquet files
- **cowplot**: Publication-quality plots
- **DataExplorer**: Automated exploratory data analysis
- **glue**: String interpolation for messages

### Analysis-Specific
- **muhaz**: Hazard function estimation
- **broom**: Tidy model outputs
- **gtsummary**: Publication-ready tables
- **conflicted**: Namespace conflict management
- **lubridate**: Date/time operations

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

---

**Last Updated**: October 29, 2025  
**Maintainer**: Mick Cooney (mcooney@describedata.com)
