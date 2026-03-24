# Bayesian Survival Analysis

This repository contains materials for performing Bayesian survival analysis using Stan and `brms` in R. The project is structured as a series of Quarto notebooks (`.qmd`) that render into interactive HTML reports.

## Table of Contents & Recommended Reading Order

Below is the recommended reading order for the rendered HTML files. This sequence provides a logical progression from initial data exploration through classical baselines and into advanced Bayesian modeling.

1. **[Explore the Lifebook Protection Data](exploration_lifebook_data.html)**  
   *Source: `exploration_lifebook_data.qmd`*  
   Initial exploratory data analysis (EDA) of the life insurance dataset. This notebook cleans, prepares, and visualizes the raw data to help understand survival times, censoring distributions, and key covariates before any modeling begins.

2. **[Perform Basic Classic Survival Models](classic_survival_models.html)**  
   *Source: `classic_survival_models.qmd`*  
   Fits traditional frequentist survival models (e.g., Kaplan-Meier curves, Cox Proportional Hazards models) to establish baselines and map out the core survival characteristics of the dataset.

3. **[Initial Bayesian Survival Models for Life Insurance Lapses](initial_bayesian_survival.html)**  
   *Source: `initial_bayesian_survival.qmd`*  
   Introduces Bayesian survival analysis using `brms` and `stan_surv()`. This notebook explores parametric and M-spline baseline hazard models on the full dataset, demonstrating how to extract and evaluate Bayesian posteriors for survival metrics.

4. **[Bayesian Survival Models for Life Insurance Lapses (Filtered Dataset)](bayesian_survival_filtered_data.html)**  
   *Source: `bayesian_survival_filtered_data.qmd`*  
   Applies refined Bayesian survival models to a specifically filtered subset of the data. This notebook focuses heavily on model diagnostics (mixing, convergence), caching strategies, and interpreting posterior predictive distributions on specific cohorts.

5. **[Conditional Survival Prediction with Cox-PH Models](conditional_survival_prediction.html)**  
   *Source: `conditional_survival_prediction.qmd`*  
   Examines conditional survival probabilities using Cox-PH models, answering practical forecasting questions such as: "Given a policy has survived $t$ months, what is the probability it survives for another $x$ months?"

6. **[Survival Analysis with Stan (Presentation)](bayesian_survival_talk.html)**  
   *Source: `bayesian_survival_talk.qmd`*  
   A Reveal.js slide deck summarizing the core methodologies, comparisons between classical and Bayesian approaches, and key findings of the project.

## Rendering the Notebooks

This project includes a `Justfile` to orchestrate rendering tasks either locally or within an isolated Podman container.

To see all available commands, run:
```bash
just
```

To build all the HTML reports using the provided Podman container, run:
```bash
just podman-all-html
```