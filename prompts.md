# Project Prompts Reference

This file contains important prompts used throughout the project development for reference and reuse.

---

## Refactor Notebook: Standardize Naming, Reorganize Shared Objects, Convert to Parquet

**Date:** November 25, 2025  
**File:** `initial_bayesian_survival.qmd`  
**Branch:** `build-init-survmodel`

### Context

During development of the Bayesian survival analysis notebook, the code evolved organically with three model iterations (lapse1, lapse2, lapse3). This led to several issues:

1. **Inconsistent naming**: Shared data objects used across all models had `lapse1_` prefixes, making it unclear they were shared resources
2. **Code duplication**: Common objects (monthly dates, inforce subset, actual cashflows) were created within the first model section rather than upfront
3. **Mixed file formats**: Data saves used RDS format, which is R-specific and less portable than parquet

The refactoring aimed to:
- Clearly distinguish shared objects (no prefix) from model-specific objects (with `lapseN_` prefix)
- Create all shared objects once in a dedicated section after data loading
- Use parquet format for tibbles (better portability, compression, and performance)
- Maintain tidyverse style conventions throughout (pipes, naming, etc.)

### The Prompt

```
Please refactor the `initial_bayesian_survival.qmd` notebook with the following changes:

1. **Standardize naming conventions**: Remove the `lapse1_` prefix from all shared data objects that are used across multiple models. These shared objects should have no model-specific prefix:
   - `lapse1_model_training_tbl` → `model_training_tbl`
   - `lapse1_model_rollforward_tbl` → `model_rollforward_tbl`
   - `lapse1_monthly_tbl` → `monthly_tbl`
   - `lapse1_inforce_tbl` → `inforce_tbl`
   - `lapse1_policy_cutdown_tbl` → `policy_cutdown_tbl`
   - `lapse1_actual_monthly_tbl` → `actual_monthly_tbl`
   - `lapse1_cashflow_week_count` → `cashflow_week_count`

   Update all references to these objects throughout the entire file (in lapse1, lapse2, and lapse3 sections). Keep the `lapseN_` prefix for model-specific objects like `lapse1_coxph_stansurv`, `lapse1_post_draws_lst`, `lapse1_sim_cashflow_tbl`, etc.

2. **Reorganize shared objects**: Move the creation of all shared data objects (monthly_tbl, inforce_tbl, policy_cutdown_tbl, cashflow_week_count, actual_cashflows_tbl, actual_monthly_tbl) to a new section called "Create Shared Data Objects" immediately after the "Load Data" section. Remove their duplicate creation from the lapse1 model section.

3. **Convert file saves to parquet**: In all `clear_large_objects` chunks, change table saves from `write_rds()` to `write_parquet()` for tibble objects. Use pipe notation (`|>`) for all write operations. Keep list objects (like `*_post_draws_lst`) as RDS files with a comment explaining why.

Make sure to maintain tidyverse style conventions throughout: use `|>` pipes, suffix all tibbles with `_tbl`, and avoid base R shortcuts.
```

### Results

This single comprehensive prompt successfully:

1. **Renamed 7 shared objects** and updated ~20+ references across all three model sections
2. **Created new "Create Shared Data Objects" section** with 6 chunks:
   - `create_monthly_dates`
   - `create_inforce_subset`
   - `create_policy_cutdown`
   - `extract_cashflow_week_count`
   - `calculate_actual_cashflows`
   - `summarise_actual_cashflows`
3. **Removed duplicate code** from lapse1 section (monthly dates setup, inforce filtering, actual cashflow calculation)
4. **Converted 4 tibble saves** from RDS to parquet format (lapse1 and lapse2 post_survival and sim_cashflow tables)
5. **Updated all write operations** to use pipe notation (`object |> write_parquet(path)`)
6. **Added explanatory comments** for why list objects remain as RDS

### Lessons Learned

- **Front-load shared resources**: Create common objects in a dedicated section before model-specific code
- **Clear naming conventions**: Prefix distinguishes scope (shared vs model-specific)
- **Comprehensive prompts work**: A well-structured single prompt is more efficient than iterative changes
- **Specify style preferences**: Mentioning tidyverse conventions ensures consistent code style
- **Format considerations**: Parquet for tibbles, RDS for complex nested structures

### Related Files

- Main notebook: `initial_bayesian_survival.qmd`
- Style guide: `AGENTS.md`
- Copilot instructions: `.github/copilot-instructions.md`

---

