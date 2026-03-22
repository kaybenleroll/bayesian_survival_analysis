# Agent Guidelines for Bayesian Survival Analysis Project

## 1. Naming & Style Conventions
- **Tibbles/Plots/Models**: Suffix with `_tbl`, `_plot`, `_km`, `_coxph`, etc.
- **Shared vs Model-Specific**:
  - **Shared objects** (reused across models): **NO prefix** (e.g., `model_training_tbl`, `monthly_tbl`).
  - **Model-Specific objects**: **Use prefix** matching the model (e.g., `lapse1_coxph_stansurv`, `filterlapse2_post_draws_lst`).
- **Parentheses Indentation**: The closing `)` must be indented **exactly 2 spaces** relative to the line containing the opening `(`. This applies to all functions and `ggplot` layers.

## 2. Critical Domain Rules
- **Age Calculations**: **NEVER use `age_life1`** (it is static). **ALWAYS calculate age dynamically from `dob_life1`** using `time_length(difftime())` relative to the current evaluation point.

## 3. Quarto Formatting & Execution
- **Output Formatting**: Always use `write_lines(text, stdout())` for text output in chunks to ensure proper rendering.
- **Chunk Timing**: Add automatic timing using knitr hooks (defined in notebooks) to output to `chunk_timings/`.
- **Stan Output**: Use `#| results: hide` for console-only Stan progress. Set `refresh = 200` in `stan_surv()`.

## 4. Data Persistence & Caching
- **Tabular Data**: Use `write_parquet_compressed(path)` wrapper (zstd level 3) from `lib_utils.R`. Always use the pipe: `obj |> write_parquet_compressed(...)`.
- **Complex Objects (Lists, Models)**: Use `qs` package. Call `qsave(path, preset = "balanced")`.
- **Manual Caching Mandate**: For large objects (>2GB) like posterior draws, **DO NOT use Quarto's built-in `#| cache: true`** (causes "long vectors not supported" errors). Implement manual caching using `if (file_exists(cache_file)) { qread() } else { compute(); qsave() }`.

## 5. Custom Libraries (Do not reconstruct internally)
- **`lib_brms_hazard.R`**: Extracts baseline hazards directly from brms' stored basis (`brmsfit_obj$basis$dpars$mu$bhaz$basis_matrix`). No exponentiation needed for `sbhaz`.
- **`lib_survival_modelling.R`**: Extracts M-spline baseline hazards from rstanarm `stan_surv()` objects (`extract_stansurv_baseline_hazard`) and compares them with frequentist `coxph` models (`compare_baseline_hazards`).
- **`lib_stan_diagnostics.R`**: Use `plot_stan_convergence()` and `plot_stan_mixing()` (auto-selects parameters with lowest n_eff) for MCMC diagnostics. Functions use bare package names.

## 6. Infrastructure Quirks
- **Justfile**: Version 1.40.0 has period-parsing issues. Use underscores in target names (e.g., `just exploration_lifebook_data`, not `.html`).
- **Podman**: Requires `--userns=keep-id` (for file ownership) and `RUNROOTLESS=false` for RStudio Server to function properly. Volume mount requires the `:z` SELinux flag.