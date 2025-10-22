# Study Results — Folder Guide

This README describes the contents of the `results` folder to accompany the manuscript and source code.

## Top-level layout
- `Bayes/` — Outputs from Bayesian multilevel models (brms/Stan).
- `LME with ML/` — Linear mixed-effects models fitted by **maximum likelihood** (lmer/lmerTest), including bootstrap CIs and model R².
- `LME with REML/` — Linear mixed-effects models fitted by **REML** (lmer/lmerTest).
- `IRR/` — Inter-rater reliability (Fleiss' κ and pairwise Cohen’s κ).

---

## Conventions used across files
- **Outcomes**: `calmness`, `self_esteem`, `valence`.
- **Event categories** (where applicable): *Daily Routines and Household Activities*, *Leisure and Recreation*, *Social and Personal Relationships*, *Work and Professional Engagements*, *Health and Well-being*, *Indeterminate*.
- **Grouping variables** seen in outputs: `group` (integer codes), random intercept `id`.
- **Model identifiers**: `model_1`, `model_2` (two model specifications; see manuscript for definitions).
- Unless noted, tables are comma‑separated values (CSV).

---

## `Bayes/`
Bayesian hierarchical model outputs.

**Files & patterns**
- `model_*_{calmness,self_esteem,valence}_summary.csv`  
  Posterior summaries of fixed effects. Columns typically include: `Parameter`, `Estimate`, `Est.Error`, `l-95% CI`, `u-95% CI`, `Rhat`, `Bulk_ESS`, `Tail_ESS`.
- `model_*_{calmness,self_esteem,valence}_summary.txt`  
  Formatted `brms` summaries (family/links, formula, draws, random effects, fixed effects).
- `model_*_{calmness,self_esteem,valence}_ppc_summary.csv`  
  Posterior predictive checks by `group` and `event_category` with columns: `Mean`, `Median`, `SD`, `ETI 2.5%`, `ETI 97.5%`, `HDI 2.5%`, `HDI 97.5%`.
- `combined_mcmc_diagnostics_b_Intercept.png`  
  MCMC diagnostics figure for the intercept term (use to inspect mixing and convergence).

**Notes**
- Random‑effects structure includes a subject‐level intercept: `(1 | id)`.
- Use these outputs for effect estimates, uncertainty, and PPC summaries.

---

## `LME with ML/`
Frequentist linear mixed‑effects models fitted by maximum likelihood.

**Files & patterns**
- `outputmodel_summary_RQ{1,2,3}_{calmness,self_esteem,valence}.txt`  
  `lmer(lmerTest)` model summaries (AIC/BIC, random‑effects variance components, Satterthwaite t‑tests for fixed effects).
- `bootstrap_results_RQ{1,2,4}_{calmness,self_esteem,valence}.csv`  
  Bootstrap confidence intervals for fixed effects. Columns: `Coefficient`, `Mean`, `CI_Lower`, `CI_Upper`.
- `Combined_R2_Results.csv`  
  Marginal and conditional R² per model. **Semicolon‑delimited** with **comma decimal separator** (European format).

**Read hints**
- R: `read.csv2("LME with ML/Combined_R2_Results.csv")`  
- Python: `pd.read_csv(".../Combined_R2_Results.csv", sep=";")` (convert comma decimals if needed).

---

## `LME with REML/`
Frequentist linear mixed‑effects models fitted by restricted maximum likelihood.

**Files & patterns**
- `outputmodel_summary_RQ{1,2,3}_{calmness,self_esteem,valence}.txt`  
  `lmer(lmerTest)` summaries analogous to ML fits, but estimated with REML.
- (No bootstrap or R² tables in this folder.)

**Use**
- Prefer REML summaries for unbiased variance‑component estimation; prefer ML for likelihood‑based model comparison.

---

## `IRR/`
Inter‑rater reliability for the qualitative coding scheme.

**Files**
- `fleiss_kappa_results.txt` — Overall Fleiss’ κ with components (P̄, Pₑ), item‑wise agreement for a sample, and category proportions.
- `fleiss_kappa_ci_results.txt` — Confidence interval for Fleiss’ κ (analytic).
- `bootstrap_fleiss_kappa_ci_results.txt` — Bootstrap confidence interval for Fleiss’ κ.
- `pairwise_cohens_kappa_matrix.csv` — Symmetric matrix of pairwise Cohen’s κ between raters (diagonal = 1).

---
