# Results Folder README

## Overview

This folder contains the statistical output files supporting the manuscript:

**Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: Contextual Insights from Large Language Models in Ambulatory Assessment**

The directory is intended for repository deposition alongside the associated manuscript and analysis scripts. It provides organised model outputs for the principal analyses, supplementary sensitivity analyses, Bayesian models, bootstrap estimates, covariance-structure checks, and scale reliability analyses.

## Author information

**David Levi Tekampe**  
PhD Researcher  
University of Luxembourg  
Department of Behavioural and Cognitive Science

## Contents of the results directory

### `MixedEffects/`
Contains the primary linear mixed-effects model summaries and model-level variance explained.

Key files include:
- `model_summary_RQ1_*` — primary mixed-effects models for outcomes by event category and group.
- `model_summary_RQ2_*` — mixed-effects models including sentiment-related predictors.
- `model_summary_RQ3_*` — exploratory mixed-effects models using event subcategories.
- Additional numbered summaries (for example, `RQ11`, `RQ12`, `RQ111`, and `RQ222`) correspond to extended and sensitivity analyses generated within the mixed-effects workflow.
- `Combined_R2_Results_all_models.csv` — marginal and conditional \(R^2\) values across fitted mixed-effects models.

Outcomes are reported separately for:
- `self_esteem`
- `valence`
- `calmness`

### `Bayes/`
Contains Bayesian model summaries exported as fixed-effects tables for the same substantive model families as the mixed-effects analyses.

Files follow the pattern:
- `model_1_*_fixed_effects.csv`
- `model_2_*_fixed_effects.csv`
- `model_3_*_fixed_effects.csv`

These files report posterior estimates, uncertainty intervals, convergence indices, and effective sample size diagnostics for each fixed effect.

### `Bootstrap/`
Contains bootstrap summaries for the mixed-effects models.

Files follow the pattern:
- `bootstrap_results_RQ1_*`
- `bootstrap_results_RQ2_*`
- `bootstrap_results_RQ3_*`

Each file includes the bootstrap mean estimate and percentile-based 95% confidence interval for each coefficient.

### `Covariate/`
Contains covariate-adjusted mixed-effects model summaries used as sensitivity analyses.

Files follow the pattern:
- `model_summary_RQ11_*`
- `model_summary_RQ12_*`

These models extend the primary analyses by adjusting for demographic covariates included in the analytical workflow.

### `Covariance_Structure_Selection/`
Contains sensitivity analyses evaluating alternative residual covariance structures for selected mixed-effects models.

Included outputs comprise:
- model-specific fit tables
- likelihood-ratio test summaries
- comparisons of fixed effects between baseline and best-fitting covariance structures
- combined summary tables across models

Representative files include:
- `combined_covstruct_fit_tables.csv`
- `combined_covstruct_lrt_vs_ind.csv`
- `combined_sensitivity_changed_terms_summary.csv`

### Top-level file
- `reliability_summary.csv` — summary reliability estimates for key two-item affect scales.

## Analytical overview

The results are organised around three principal model families:

### Model 1
Primary mixed-effects models examining associations between **event category**, **group**, and their interaction for the outcomes:
- self-esteem
- valence
- calmness

### Model 2
Mixed-effects models extending Model 1 by incorporating **sentiment-related predictors**, including within-person and between-person sentiment components.

### Model 3
Exploratory mixed-effects models examining **event subcategories** in relation to the same outcomes.

In addition to these principal analyses, the folder includes:
- Bayesian counterparts to the main model families
- bootstrap-based robustness checks
- covariate-adjusted sensitivity models
- covariance-structure selection diagnostics
- reliability estimates for scale components

## File naming conventions

- `RQ1`, `RQ2`, `RQ3` refer to the main research-question model families.
- Suffixes such as `self_esteem`, `valence`, and `calmness` identify the dependent variable.
- Files labelled `fixed_effects` contain parameter-level summaries.
- Files labelled `model_summary` contain full text summaries of fitted models.
- Files labelled `bootstrap_results` contain bootstrap-based coefficient summaries.
- Files labelled `fit_table`, `lrt_vs_ind`, and `fixed_effects_compare_lmer_vs_best` belong to covariance-structure sensitivity analyses.

## File formats

The folder contains plain-text (`.txt`) and comma- or semicolon-delimited (`.csv`) files generated directly from the analytical workflow in R. Delimiters may differ across files because different export functions were used during model generation.

## Reproducibility note

These files are derived outputs and should be interpreted together with the accompanying analysis scripts and manuscript. The directory does **not** contain raw participant data. Reproduction of the results requires access to the underlying dataset and the corresponding R analysis scripts.

## Recommended citation of this results folder

When referring to this directory in a repository or supplement, it may be described as:

> Statistical output files supporting the manuscript by David Levi Tekampe, University of Luxembourg, Department of Behavioural and Cognitive Science.

