
# Linear Mixed-Effects Analyses 
# LME, Bootstrap, Marginal and Conditional R2, Plots
#
# Notes
# - Expects CSVs with semicolon separators and UTF-8 encoding.
# - Saves outputs
#
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#
# Author: David Levi Tekampe
# University of Luxembourg

log_conn <- file("<path to logfile>", open = "wt")
sink(log_conn, type = "output")
sink(log_conn, type = "message")

install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    message(paste("Installing package:", package_name))
    tryCatch({
      install.packages(package_name, dependencies = TRUE)
      message(paste("Package", package_name, "installed successfully."))
      library(package_name, character.only = TRUE)
      message(paste("Package", package_name, "loaded."))
    }, error = function(e) {
      message(paste("Error installing or loading package:", package_name))
      message(paste("Error message:", e$message))
    })
  } else {
    message(paste("Package", package_name, "is already installed. Loading..."))
    library(package_name, character.only = TRUE)
    message(paste("Package", package_name, "loaded."))
  }
}

required_packages <- c("dplyr", "lme4", "pROC", "ggplot2", "car", "knitr", "ggeffects", "viridis", "lmerTest", "ggdist", "ggpubr", "lattice", "lubridate", "ggpredict", "papaja", "grid", "gridExtra", "magrittr", "forecast", "sjPlot", "webshot", "MuMIn", "nlme")

invisible(lapply(required_packages, install_and_load_package))

file_path <- "<path to input CSV file>"
setwd(dirname(file_path))
df <- read.csv(file_path, sep = ";", encoding = "utf-8")

df$id <- factor(df$id)


df$event_category <- factor(
  df$event_category,
  levels = c(
    "Daily Routines and Household Activities",
    "Leisure and Recreation",
    "Social and Personal Relationships",
    "Work and Professional Engagements",
    "Health and Well-being",
    "Indeterminate"
  )
)

df$event_category <- relevel(df$event_category, ref = "Daily Routines and Household Activities")

df$event_subcategory <- factor(df$event_subcategory, levels = c(
  "Entertainment and Media Consumption",
  "Personal Care and Hygiene",
  "Commuting",
  "Meals and Snacks",
  "Family Dynamics",
  "Socializing and Leisure",
  "Travel and Excursions",
  "Cultural and Community Events",
  "Companion Animal Care",
  "Outdoor and Recreational Activities",
  "Rest and Sleep",
  "Medical Treatment and Healthcare Interactions",
  "Healthcare Journeys and Visits",
  "Shopping and Errands",
  "Household Chores",
  "Emotional and Conflict Resolution",
  "Work-Related Tasks and Projects",
  "Mental Health and Emotional State",
  "Indeterminate",
  "Romantic Partnerships",
  "Mood and Emotional Fluctuations",
  "Friendship Activities",
  "Acquaintance and Casual Interactions",
  "Educational and Academic Work",
  "Professional Communication and Meetings",
  "Relaxation",
  "Leisure and Entertainment at Home",
  "Reading and Intellectual Activities",
  "Games and Hobbies",
  "Financial and Administrative Tasks",
  "Physical Health and Symptoms",
  "Sports and Physical Activities"
))

df$event_subcategory <- relevel(df$event_subcategory, ref = "Meals and Snacks")

df$event_sentiment <- factor(
  df$event_sentiment,
  levels = c("negative", "neutral", "positive", "none")
)

df$event_sentiment <- relevel(df$event_sentiment, ref = "neutral")

if (!"event_sentiment_encoded" %in% names(df)) {
  stop("Column 'event_sentiment_encoded' not found in df. It should code sentiment as -1, 0, 1, NA.")
}

df$event_sentiment_encoded <- as.numeric(df$event_sentiment_encoded)

df <- df %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    sentiment_mean   = mean(event_sentiment_encoded, na.rm = TRUE),   
    sentiment_within = event_sentiment_encoded - sentiment_mean       
  ) %>%
  dplyr::ungroup()

df$ediary_type <- factor(df$ediary_type, levels = c("1", "2"))
df$ediary_type <- relevel(df$ediary_type, ref = "2")

df$group <- factor(df$group, levels = c("1", "2"))
df$group <- relevel(df$group, ref = "2")

df$family_status <- factor(
  df$family_status,
  levels = c(
    "single",
    "cohabiting relationship",
    "married",
    "living separately",
    "divorced"
  )
)

df$highest_education <- factor(
  df$highest_education,
  levels = c(
    "none",
    "secondary school",
    "intermediate school",
    "university entrance qualification",
    "others"
  )
)

df$work_status <- factor(
  df$work_status,
  levels = c(
    "employed",
    "registered unemployed",
    "otherwise unemployed",
    "training re-education",
    "disability pension",
    "ill",
    "housewife",
    "sheltered employed",
    "others"
  )
)

demographic_vars <- c("age", "family_status", "highest_education", "work_status")

df$BPD_severity <- df$bpdanz

if (!"BSL_T1_mean" %in% names(df)) {
  stop("Column 'BSL_T1_mean' not found in df. Did you load <path to input CSV file>?")
}

df$BSL_score <- as.numeric(df$BSL_T1_mean)

bsl_nunique <- tapply(df$BSL_score, df$id, function(x) length(unique(na.omit(x))))
if (any(bsl_nunique > 1, na.rm = TRUE)) {
  warning("BSL_score varies within some ids. Please check the merge.")
}

variables_list <- c(
  "prompt", "day",
  "event_evaluation_centered",
  "event_sentiment",
  "event_sentiment_encoded",
  "sentiment_within",
  "sentiment_mean",
  "self_esteem",
  "valence",
  "calmness",
  "id",
  "group",
  "event_category",
  "ediary_type",
  "event_subcategory",
  demographic_vars,
  "BSL_score",
  "BPD_severity"
)

analysis_df <- df[variables_list]
analysis_df$id <- factor(analysis_df$id)
analysis_df <- analysis_df[order(row.names(analysis_df)), ]

output_base_path <- "<path to output directory>"

unique_ids <- unique(analysis_df$id)
print(unique_ids)

print(class(analysis_df$id))

data_types <- sapply(analysis_df, class)
print(data_types)

summary(analysis_df)

analysis_df <- analysis_df %>%
  dplyr::mutate(
    day = dplyr::if_else(is.na(day), as.numeric((prompt - 1) %/% 12 + 1), as.numeric(day)),
    day = factor(day),

    prompt_in_day = (prompt - 1) %% 12 + 1
  )


two_item_alpha_sb <- function(r) ifelse(is.na(r), NA_real_, (2*r)/(1+r))

compute_reliabilities_2item <- function(dat, id, item_pos, item_rev, kmax = 6) {
  d <- dat %>%
    dplyr::select({{id}}, {{item_pos}}, {{item_rev}}) %>%
    dplyr::rename(i_pos = {{item_pos}}, i_neg = {{item_rev}}) %>%
    dplyr::mutate(i_neg = kmax - i_neg)  
  
  r_overall <- suppressWarnings(cor(d$i_pos, d$i_neg, use = "pairwise.complete.obs"))
  a_overall <- two_item_alpha_sb(r_overall)
  
  m <- d %>%
    dplyr::group_by({{id}}) %>%
    dplyr::summarise(i_pos = mean(i_pos, na.rm = TRUE),
                     i_neg = mean(i_neg, na.rm = TRUE),
                     .groups = "drop")
  r_between <- suppressWarnings(cor(m$i_pos, m$i_neg, use = "pairwise.complete.obs"))
  a_between <- two_item_alpha_sb(r_between)
  
  dc <- d %>%
    dplyr::group_by({{id}}) %>%
    dplyr::mutate(i_pos = i_pos - mean(i_pos, na.rm = TRUE),
                  i_neg = i_neg - mean(i_neg, na.rm = TRUE)) %>%
    dplyr::ungroup()
  r_within <- suppressWarnings(cor(dc$i_pos, dc$i_neg, use = "pairwise.complete.obs"))
  a_within <- two_item_alpha_sb(r_within)
  
  tibble::tibble(
    inter_item_r_overall = r_overall, alpha_overall = a_overall,
    inter_item_r_between = r_between, alpha_between = a_between,
    inter_item_r_within  = r_within,  alpha_within  = a_within
  )
}

rel_valence <- compute_reliabilities_2item(
  df, id = id, item_pos = satisfied, item_rev = unwell, kmax = 6
) %>% dplyr::mutate(scale = "Valence (well & satisfied)")

rel_calmness <- compute_reliabilities_2item(
  df, id = id, item_pos = relaxed, item_rev = restless, kmax = 6
) %>% dplyr::mutate(scale = "Calmness / tense-arousal (relaxed & low restless)")

reliab_out <- dplyr::bind_rows(rel_valence, rel_calmness) %>%
  dplyr::select(scale,
                inter_item_r_overall, alpha_overall,
                inter_item_r_between, alpha_between,
                inter_item_r_within,  alpha_within)

readr::write_csv(reliab_out, file.path(output_base_path, "reliability_summary.csv"))
print(reliab_out)



install_and_load_package("multilevelTools")
df <- df %>%
  dplyr::mutate(
    se_useless_r = 9 - .data$self_esteem_02_useless,
    se_failure_r = 9 - .data$self_esteem_03_failure
  )

omega_se <- multilevelTools::omegaSEM(
  items = c("self_esteem_01_satisfied", "self_esteem_04_positive_attitude",
            "se_useless_r", "se_failure_r"),
  id = "id",
  data = df,
  savemodel = FALSE
)
print(omega_se)

df$self_esteem_pos <- rowMeans(
  cbind(df$self_esteem_01_satisfied,
        df$se_useless_r,
        df$se_failure_r,
        df$self_esteem_04_positive_attitude),
  na.rm = TRUE
)

format_p_apa_exact <- function(p, digits_default = 3, digits_max = 8) {
  if (is.na(p)) return(NA_character_)
  p <- as.numeric(p)
  if (is.na(p)) return(NA_character_)
  
  if (p < 0.001) return("< .001")
  
  s <- formatC(p, format = "f", digits = digits_default)
    
  if (p < 1) s <- sub("^0", "", s)
  s
}

create_and_summarize_save_model <- function(dependent_var, formula_part, data, output_base_path, rq_number) {
  data$group <- factor(data$group, levels = c("1", "2"), labels = c("BPD", "HC"))
  data$group <- relevel(data$group, ref = "HC")
  
  formula <- as.formula(paste(dependent_var, formula_part))
  model <- lmerTest::lmer(formula, data = data, REML = FALSE)
  
  model_summary <- summary(model)
  
  coef_df <- model_summary$coefficients %>%
    as.data.frame() %>%
    dplyr::rename(
      "b"  = "Estimate",
      "SE" = "Std. Error",
      "t"  = "t value",
      "p"  = "Pr(>|t|)"
    ) %>%
    dplyr::mutate(
      Effect = papaja::beautify_terms(rownames(.)),
      p = vapply(as.numeric(p), format_p_apa_exact, character(1))
    )
  
  num_fmt <- papaja::printnum(
    dplyr::select(coef_df, b, SE, t, df),
    digits = c(4, 4, 4, 4),
    gt1    = c(TRUE, TRUE, TRUE, TRUE),
    zero   = c(TRUE, TRUE, TRUE, TRUE)
  )
  
  apa_table <- dplyr::bind_cols(
    dplyr::select(coef_df, Effect),
    num_fmt,
    dplyr::select(coef_df, p)
  ) %>%
    dplyr::select(Effect, `b`, `SE`, `t`, `df`, `p`)
  
  file_name_txt <- paste0(output_base_path, 'model_summary_RQ', rq_number, '_', dependent_var, '.txt')
  sink(file = file_name_txt)
  cat("Model Summary for RQ", rq_number, " (", dependent_var, "):\n")
  print(model_summary)
  cat("\nAPA Style Table:\n")
  print(apa_table)
  sink()
  
  file_name_png <- paste0(output_base_path, 'APA_table_RQ', rq_number, '_', dependent_var, '.png')
  png(file_name_png, width = 12, height = 6, units = 'in', res = 150)
  grid.table(apa_table)
  dev.off()

  file_name_csv <- paste0(output_base_path, 'APA_table_RQ', rq_number, '_', dependent_var, '.csv')
  write.csv2(apa_table, file = file_name_csv, row.names = FALSE, fileEncoding = "UTF-8")
  
  return(model)
}

create_and_summarize_save_model_bpd <- function(dependent_var, formula_part, data, output_base_path, rq_number) {
  formula <- as.formula(paste(dependent_var, formula_part))
  model <- lmerTest::lmer(formula, data = data, REML = FALSE)
  
  model_summary <- summary(model)
  
  coef_df <- model_summary$coefficients %>%
    as.data.frame() %>%
    dplyr::rename(
      "b"  = "Estimate",
      "SE" = "Std. Error",
      "t"  = "t value",
      "p"  = "Pr(>|t|)"
    ) %>%
    dplyr::mutate(
      Effect = papaja::beautify_terms(rownames(.)),
      p = vapply(as.numeric(p), format_p_apa_exact, character(1))
    )
  
  num_fmt <- papaja::printnum(
    dplyr::select(coef_df, b, SE, t, df),
    digits = c(4, 4, 4, 4),
    gt1    = c(TRUE, TRUE, TRUE, TRUE),
    zero   = c(TRUE, TRUE, TRUE, TRUE)
  )
  
  apa_table <- dplyr::bind_cols(
    dplyr::select(coef_df, Effect),
    num_fmt,
    dplyr::select(coef_df, p)
  ) %>%
    dplyr::select(Effect, `b`, `SE`, `t`, `df`, `p`)
  
  file_name_txt <- paste0(output_base_path, "model_summary_RQ", rq_number, "_", dependent_var, ".txt")
  sink(file = file_name_txt)
  cat("Model Summary for RQ", rq_number, " (", dependent_var, "):\n")
  print(model_summary)
  cat("\nAPA Style Table:\n")
  print(apa_table)
  sink()
  
  file_name_png <- paste0(output_base_path, "APA_table_RQ", rq_number, "_", dependent_var, ".png")
  png(file_name_png, width = 12, height = 6, units = "in", res = 150)
  grid.table(apa_table)
  dev.off()
  
  file_name_csv <- paste0(output_base_path, "APA_table_RQ", rq_number, "_", dependent_var, ".csv")
  write.csv2(apa_table, file = file_name_csv, row.names = FALSE, fileEncoding = "UTF-8")
  
  return(model)
}


install_and_load_package("nlme")
install_and_load_package("lmerTest")
install_and_load_package("dplyr")

.say <- function(...) { cat(..., "\n"); flush.console() }

extract_fixef_lmer <- function(m) {
  sm <- summary(m)
  tab <- as.data.frame(sm$coefficients)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  
  tab <- tab %>%
    dplyr::rename(
      b  = .data$Estimate,
      SE = .data$`Std. Error`,
      df = .data$df,
      t  = .data$`t value`,
      p  = .data$`Pr(>|t|)`
    ) %>%
    dplyr::select(.data$term, .data$b, .data$SE, .data$t, .data$df, .data$p)
  tab
}

extract_fixef_nlme <- function(m) {
  sm <- summary(m)
  tab <- as.data.frame(sm$tTable)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  tab <- tab %>%
    dplyr::rename(
      b  = .data$Value,
      SE = .data$`Std.Error`,
      df = .data$DF,
      t  = .data$`t-value`,
      p  = .data$`p-value`
    ) %>%
    dplyr::select(.data$term, .data$b, .data$SE, .data$t, .data$df, .data$p)
  tab
}

get_cor_param <- function(m) {
  cs <- tryCatch(m$modelStruct$corStruct, error = function(e) NULL)
  if (is.null(cs)) return(NA_real_)
  out <- tryCatch(nlme::coef(cs, unconstrained = FALSE), error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(unlist(out)))
  if (length(out) == 0) return(NA_real_)
  out[1]
}

compare_cov_structures_and_refit <- function(
    dependent_var,
    fixed_part,
    data,
    output_path,
    model_label,
    min_n_per_day = 3,
    start_vals_ar = c(0.10, 0.20, 0.30, 0.40, 0.50),
    random_structure = c("id", "id_day"),
    alpha = 0.05
) {
  random_structure <- match.arg(random_structure)
  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  d <- data
  
  req <- c("id", "day", "prompt_in_day", dependent_var)
  miss <- setdiff(req, names(d))
  if (length(miss) > 0) stop("Data missing required columns: ", paste(miss, collapse = ", "))
  
  if ("group" %in% names(d)) {
    d$group <- factor(d$group, levels = c("1", "2"), labels = c("BPD", "HC"))
    d$group <- stats::relevel(d$group, ref = "HC")
  }
  
  d$id <- factor(d$id)
  d$day_ar1 <- factor(d$day)
  
  fixed_formula_nlme <- stats::as.formula(paste(dependent_var, fixed_part))
  
  needed_vars <- unique(c(all.vars(fixed_formula_nlme), "id", "day_ar1", "prompt_in_day"))
  d2 <- d[, needed_vars, drop = FALSE]
  d2 <- d2[stats::complete.cases(d2), ]
  d2 <- droplevels(d2)
  
  d2 <- d2[order(d2$id, d2$day_ar1, d2$prompt_in_day), ]
  
  d2$id_day <- interaction(d2$id, d2$day_ar1, drop = TRUE)
  tab_n <- table(d2$id_day)
  keep_days <- names(tab_n)[tab_n >= min_n_per_day]
  d2 <- d2[d2$id_day %in% keep_days, ]
  d2 <- droplevels(d2)
  
  if (nrow(d2) == 0) stop("No data left after filtering (check missingness and min_n_per_day).")
  
  random_formula_nlme <- switch(
    random_structure,
    "id"     = ~ 1 | id,
    "id_day" = ~ 1 | id/day_ar1
  )
  
  random_part_lmer <- switch(
    random_structure,
    "id"     = "(1 | id)",
    "id_day" = "(1 | id/day_ar1)"
  )
  
  lmer_formula <- stats::as.formula(
    paste(dependent_var, fixed_part, "+", random_part_lmer)
  )
  
  ctrl <- nlme::lmeControl(
    returnObject = TRUE,
    msMaxIter = 200,
    msMaxEval = 200,
    opt = "optim"
  )
  
  fit_lme <- function(correlation = NULL) {
    nlme::lme(
      fixed = fixed_formula_nlme,
      random = random_formula_nlme,
      correlation = correlation,
      data = d2,
      method = "ML",
      control = ctrl
    )
  }
  
  fit_ar_family <- function(struct = c("AR1", "CAR1")) {
    struct <- match.arg(struct)
    last_err <- NULL
    for (s in start_vals_ar) {
      cor_struct <- if (struct == "AR1") {
        nlme::corAR1(form = ~ prompt_in_day | id/day_ar1, value = s)
      } else {
        nlme::corCAR1(form = ~ prompt_in_day | id/day_ar1, value = s)
      }
      m <- tryCatch(fit_lme(correlation = cor_struct), error = function(e) e)
      if (!inherits(m, "error")) return(m)
      last_err <- m
    }
    stop(last_err$message)
  }
  
  m_lmer <- lmerTest::lmer(lmer_formula, data = d2, REML = FALSE)
  
  fits <- list()
  fits$Independent <- tryCatch(fit_lme(NULL), error = function(e) e)
  fits$CS <- tryCatch(
    fit_lme(nlme::corCompSymm(form = ~ 1 | id/day_ar1)),
    error = function(e) e
  )
  fits$AR1 <- tryCatch(fit_ar_family("AR1"), error = function(e) e)
  fits$CAR1 <- tryCatch(fit_ar_family("CAR1"), error = function(e) e)
  
  ok <- vapply(fits, function(x) !inherits(x, "error"), logical(1))
  fits_ok <- fits[ok]
  if (length(fits_ok) == 0) stop("All covariance-structure fits failed for model: ", model_label)
  
  fit_table <- do.call(
    rbind,
    lapply(names(fits_ok), function(nm) {
      m <- fits_ok[[nm]]
      data.frame(
        model_label = model_label,
        structure = nm,
        random_structure = random_structure,
        AIC = AIC(m),
        BIC = BIC(m),
        logLik = as.numeric(logLik(m)),
        n_obs = nrow(nlme::getData(m)),
        cor_param = if (nm == "Independent") NA_real_ else get_cor_param(m),
        stringsAsFactors = FALSE
      )
    })
  )
  fit_table <- fit_table[order(fit_table$AIC), ]
  
  best_aic <- fit_table$structure[which.min(fit_table$AIC)]
  best_bic <- fit_table$structure[which.min(fit_table$BIC)]
  
  lrt_vs_ind <- data.frame()
  if ("Independent" %in% names(fits_ok)) {
    m0 <- fits_ok$Independent
    for (nm in setdiff(names(fits_ok), "Independent")) {
      m1 <- fits_ok[[nm]]
      a <- tryCatch(anova(m0, m1), error = function(e) NULL)
      if (!is.null(a) && nrow(a) >= 2) {
        lrt_vs_ind <- rbind(
          lrt_vs_ind,
          data.frame(
            model_label = model_label,
            random_structure = random_structure,
            compare = paste0(nm, " vs Independent"),
            LRT = a$L.Ratio[2],
            df = a$df[2] - a$df[1],
            p = a$"p-value"[2],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  m_best <- fits_ok[[best_aic]]
  
  fe_lmer <- extract_fixef_lmer(m_lmer) %>%
    dplyr::rename_with(~paste0(., "_lmer"), c("b","SE","t","df","p"))
  
  fe_best <- extract_fixef_nlme(m_best) %>%
    dplyr::rename_with(~paste0(., "_best"), c("b","SE","t","df","p"))
  
  comp <- dplyr::full_join(fe_lmer, fe_best, by = "term") %>%
    dplyr::mutate(
      sig_lmer = .data$p_lmer < alpha,
      sig_best = .data$p_best < alpha,
      sig_changed = dplyr::if_else(
        is.na(.data$sig_lmer) | is.na(.data$sig_best),
        NA,
        .data$sig_lmer != .data$sig_best
      ),
      sign_changed = dplyr::if_else(
        is.na(.data$b_lmer) | is.na(.data$b_best),
        NA,
        sign(.data$b_lmer) != sign(.data$b_best)
      ),
      delta_b = .data$b_best - .data$b_lmer,
      delta_SE = .data$SE_best - .data$SE_lmer
    )
  
  changed_terms <- comp %>%
    dplyr::filter(isTRUE(sig_changed) | isTRUE(sign_changed))
  
  .say("============================================================")
  .say("Covariance structure comparison + sensitivity refit for: ", model_label)
  .say("Fixed (nlme): ", deparse(fixed_formula_nlme))
  .say("Random structure (held constant): ", random_structure)
  .say("Correlation grouping: id/day_ar1 ; time = prompt_in_day")
  .say("Min obs per person-day kept: ", min_n_per_day)
  .say("------------------------------------------------------------")
  print(fit_table, row.names = FALSE)
  .say("------------------------------------------------------------")
  .say("Best (AIC): ", best_aic, " | Best (BIC): ", best_bic)
  .say("------------------------------------------------------------")
  .say("Fixed-effects comparison: lmer (ML, indep residuals) vs nlme (best = ", best_aic, ")")
  .say("Terms with changed significance (alpha=", alpha, ") and/or sign: ", nrow(changed_terms))
  if (nrow(changed_terms) > 0) {
    print(changed_terms %>% dplyr::select(term, b_lmer, p_lmer, b_best, p_best, sig_changed, sign_changed),
          row.names = FALSE)
  }
  
  failed <- names(fits)[!ok]
  if (length(failed) > 0) {
    .say("------------------------------------------------------------")
    .say("Failed to converge / fit: ", paste(failed, collapse = ", "))
    for (nm in failed) .say("  - ", nm, " error: ", fits[[nm]]$message)
  }
  
  if (nrow(lrt_vs_ind) > 0) {
    .say("------------------------------------------------------------")
    .say("LRTs vs Independent (ML fits):")
    print(lrt_vs_ind, row.names = FALSE)
  }
  .say("============================================================")
  
  out_base <- file.path(output_path, paste0("covstruct_", model_label))
  
  utils::write.csv2(fit_table, paste0(out_base, "_fit_table.csv"),
                    row.names = FALSE, fileEncoding = "UTF-8")
  
  if (nrow(lrt_vs_ind) > 0) {
    utils::write.csv2(lrt_vs_ind, paste0(out_base, "_lrt_vs_ind.csv"),
                      row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  utils::write.csv2(comp, paste0(out_base, "_fixed_effects_compare_lmer_vs_best.csv"),
                    row.names = FALSE, fileEncoding = "UTF-8")
  
  if (nrow(changed_terms) > 0) {
    utils::write.csv2(changed_terms, paste0(out_base, "_changed_terms.csv"),
                      row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  sink(file = paste0(out_base, "_summary.txt"))
  cat("Covariance structure comparison + sensitivity refit\n")
  cat("Model label: ", model_label, "\n\n")
  cat("Fixed (nlme):\n", deparse(fixed_formula_nlme), "\n\n", sep = "")
  cat("Random structure: ", random_structure, "\n", sep = "")
  cat("Correlation grouping: id/day_ar1 ; time = prompt_in_day\n")
  cat("Min obs per person-day kept: ", min_n_per_day, "\n\n", sep = "")
  cat("Fit table (sorted by AIC):\n")
  print(fit_table, row.names = FALSE)
  cat("\nBest (AIC): ", best_aic, "\nBest (BIC): ", best_bic, "\n\n", sep = "")
  if (length(failed) > 0) {
    cat("Failed to converge / fit:\n")
    for (nm in failed) cat(" - ", nm, ": ", fits[[nm]]$message, "\n", sep = "")
    cat("\n")
  }
  if (nrow(lrt_vs_ind) > 0) {
    cat("LRTs vs Independent:\n")
    print(lrt_vs_ind, row.names = FALSE)
    cat("\n")
  }
  cat("Fixed-effects comparison (lmer vs nlme best):\n")
  cat("Changed terms (significance and/or sign): ", nrow(changed_terms), "\n\n", sep = "")
  if (nrow(changed_terms) > 0) {
    print(changed_terms %>% dplyr::select(term, b_lmer, p_lmer, b_best, p_best, sig_changed, sign_changed),
          row.names = FALSE)
    cat("\n")
  }
  cat("Notes:\n")
  cat("- lmer fit is ML with conditionally independent residuals.\n")
  cat("- nlme fits are ML on the same filtered dataset (complete cases; min obs per person-day).\n")
  cat("- Best structure chosen by AIC among converged candidates.\n")
  sink()
  
  list(
    fit_table = fit_table,
    lrt_vs_ind = lrt_vs_ind,
    best_aic = best_aic,
    best_bic = best_bic,
    m_lmer = m_lmer,
    m_best = m_best,
    fixed_compare = comp,
    changed_terms = changed_terms
  )
}


output_covstruct_path <- file.path(
  "<covstruct output path>",
  ""
)
if (!dir.exists(output_covstruct_path)) dir.create(output_covstruct_path, recursive = TRUE)

covstruct_specs <- list(
  list(label = "M1_self_esteem", dv = "self_esteem",
       fixed = "~ event_category + group + event_category:group"),
  list(label = "M1_valence", dv = "valence",
       fixed = "~ event_category + group"),
  list(label = "M1_calmness", dv = "calmness",
       fixed = "~ event_category + group + event_category:group"),
  
  list(label = "M2_self_esteem", dv = "self_esteem",
       fixed = "~ event_category + group + sentiment_mean + sentiment_within * group"),
  list(label = "M2_valence", dv = "valence",
       fixed = "~ event_category + group + sentiment_mean + sentiment_within * group"),
  list(label = "M2_calmness", dv = "calmness",
       fixed = "~ event_category + group + sentiment_mean + sentiment_within * group")
)

RANDOM_FOR_COVSTRUCT <- "id"  
ALPHA_SENS <- 0.05

covstruct_results <- list()
all_fit_tables <- list()
all_lrt_tables <- list()
all_changed_summaries <- list()

for (sp in covstruct_specs) {
  res <- compare_cov_structures_and_refit(
    dependent_var = sp$dv,
    fixed_part = sp$fixed,
    data = analysis_df,
    output_path = output_covstruct_path,
    model_label = sp$label,
    min_n_per_day = 3,
    random_structure = RANDOM_FOR_COVSTRUCT,
    alpha = ALPHA_SENS
  )
  
  covstruct_results[[sp$label]] <- res
  all_fit_tables[[sp$label]] <- res$fit_table
  
  if (!is.null(res$lrt_vs_ind) && nrow(res$lrt_vs_ind) > 0) {
    all_lrt_tables[[sp$label]] <- res$lrt_vs_ind
  }
  
  all_changed_summaries[[sp$label]] <- data.frame(
    model_label = sp$label,
    best_structure_AIC = res$best_aic,
    n_changed_terms = nrow(res$changed_terms),
    stringsAsFactors = FALSE
  )
}

combined_fit <- do.call(rbind, all_fit_tables)
utils::write.csv2(
  combined_fit,
  file.path(output_covstruct_path, "combined_covstruct_fit_tables.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (length(all_lrt_tables) > 0) {
  combined_lrt <- do.call(rbind, all_lrt_tables)
  utils::write.csv2(
    combined_lrt,
    file.path(output_covstruct_path, "combined_covstruct_lrt_vs_ind.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

combined_changed <- do.call(rbind, all_changed_summaries)
utils::write.csv2(
  combined_changed,
  file.path(output_covstruct_path, "combined_sensitivity_changed_terms_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

.say("DONE: Covariance selection + sensitivity refits saved to: ", output_covstruct_path)
.say("Random structure held constant = ", RANDOM_FOR_COVSTRUCT, " | alpha = ", ALPHA_SENS)
.say("Check: combined_sensitivity_changed_terms_summary.csv for whether inference changed.")


print("M1") 

model_1_self_esteem <- create_and_summarize_save_model(
  "self_esteem", "~ event_category + group + event_category:group + (1 | id)",
  analysis_df, output_base_path, 1
)
model_1_valence <- create_and_summarize_save_model(
  "valence", "~ event_category + group + (1 | id)",
  analysis_df, output_base_path, 1
)
model_1_calmness <- create_and_summarize_save_model(
  "calmness", "~ event_category + group + event_category:group + (1 | id)",
  analysis_df, output_base_path, 1
)

print("M1-END")

print("M2")

model_2_self_esteem <- create_and_summarize_save_model(
  "self_esteem", "~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id)",
  analysis_df, output_base_path, 2
)

model_2_valence <- create_and_summarize_save_model(
  "valence", "~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id)",
  analysis_df, output_base_path, 2
)

model_2_calmness <- create_and_summarize_save_model(
  "calmness", "~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id)",
  analysis_df, output_base_path, 2
)

print("M2")

print("M3")

model_3_self_esteem <- create_and_summarize_save_model(
  "self_esteem", "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3
)
model_3_valence <- create_and_summarize_save_model(
  "valence", "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3
)
model_3_calmness <- create_and_summarize_save_model(
  "calmness", "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3
)

print("M3-END")


library(ggplot2)
library(sjPlot)     
library(ggeffects)  
library(ggtext)     
library(htmltools)  

.font_family <- "Times New Roman"
.font_size   <- 12

.group_cols <- c(HC = "

.event_levels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate"
)

.event_labels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate Event"
)

.sig_legend_default <- ""

.base_theme <- function(x_text_size = 10, y_text_size = 10) {
  theme_classic() +
    theme(
      text = element_text(family = .font_family, size = .font_size),
      axis.title = element_text(family = .font_family, size = .font_size),
      axis.text.x = element_text(family = .font_family, size = x_text_size),
      axis.text.y = element_text(family = .font_family, size = y_text_size),
      legend.title = element_text(family = .font_family, size = .font_size),
      legend.text  = element_text(family = .font_family, size = .font_size),
      plot.title = ggtext::element_markdown(hjust = 0, family = .font_family, size = .font_size),
      plot.caption = element_text(
        family = .font_family,
        size   = .font_size,
        hjust  = 0,
        margin = margin(t = 12)
      ),
      plot.margin = margin(10, 10, 10, 10, "mm")
    )
}

.group_cols_bw <- c(HC = "
.group_ltys_bw <- c(HC = "solid",  BPD = "dashed")
.group_shapes_bw <- c(HC = 16,     BPD = 17)
.group_fills_bw  <- c(HC = "

.save_plot_two_versions <- function(p, filename_colour, filename_bw,
                                    width, height, dpi = 300,
                                    bw_scales = NULL) {
  ggsave(filename_colour, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  
  p_bw <- p
  if (!is.null(bw_scales)) {
    for (sc in bw_scales) p_bw <- p_bw + sc
  }
  ggsave(filename_bw, plot = p_bw, width = width, height = height, dpi = dpi, bg = "white")
  
  invisible(list(colour = p, bw = p_bw))
}

.p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

.norm_group <- function(x) {
  x <- as.character(x)
  x[x == "1"] <- "BPD"
  x[x == "2"] <- "HC"
  x
}

.get_p_m2_interaction <- function(model) {
  ct <- as.data.frame(summary(model)$coefficients)
  ct$term <- rownames(ct)
  idx <- which(grepl("sentiment_within", ct$term) & grepl("groupBPD", ct$term))
  if (length(idx) >= 1 && "Pr(>|t|)" %in% colnames(ct)) return(as.numeric(ct$`Pr(>|t|)`[idx[1]]))
  NA_real_
}

.get_m1_interaction_stars_df <- function(model, y_limits) {
  ct <- as.data.frame(summary(model)$coefficients)
  ct$term <- rownames(ct)
  
  int_rows <- grepl("^event_category", ct$term) & grepl(":group", ct$term)
  int_df <- ct[int_rows, , drop = FALSE]
  if (nrow(int_df) == 0) return(NULL)
  
  int_df$event_level <- sub("^event_category", "", sub(":group.*$", "", int_df$term))
  int_df$stars <- vapply(int_df$`Pr(>|t|)`, .p_to_stars, character(1))
  star_df <- int_df[int_df$stars != "", c("event_level", "stars"), drop = FALSE]
  if (nrow(star_df) == 0) return(NULL)
  
  names(star_df) <- c("event_category", "stars")
  star_df$event_category <- factor(as.character(star_df$event_category), levels = .event_levels)
  star_df$y <- y_limits[2] - 0.04 * diff(y_limits)
  star_df
}

.make_title_html <- function(figure_label, figure_number, figure_title, title_suffix = "") {
  HTML(paste0(
    "<span style='font-family:", .font_family, "; font-size:12pt;'><strong>",
    figure_label, " ", figure_number,
    "</strong></span><br><br>",
    "<span style='font-family:", .font_family, "; font-size:12pt;'><em>",
    figure_title, title_suffix,
    "</em></span>"
  ))
}

.file_token <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

.figure_file_stub <- function(figure_label, figure_number) {
  paste(.file_token(figure_label), .file_token(figure_number), sep = "_")
}

plot_model1_event_x_group <- function(model, dependentVar, output_base_path,
                                      plot_width, plot_height,
                                      x_text_size, y_text_size, y_limits,
                                      figure_label, figure_number, figure_title,
                                      sig_legend_text = .sig_legend_default,
                                      ribbon_alpha = 0.18,
                                      show_stars = FALSE) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("event_category", "group")))
  names(pred)[names(pred) == "x"] <- "event_category"
  pred$group <- factor(.norm_group(pred$group), levels = c("HC", "BPD"))
  pred$event_category <- factor(as.character(pred$event_category), levels = .event_levels)
  
  pred$xnum <- as.numeric(pred$event_category)
  
  star_df <- NULL
  if (isTRUE(show_stars)) {
    star_df <- .get_m1_interaction_stars_df(model, y_limits)
  }
  
  p <- ggplot(pred, aes(x = xnum, y = predicted,
                        colour = group, group = group, linetype = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = ribbon_alpha, colour = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    .base_theme(x_text_size = x_text_size, y_text_size = y_text_size) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
      plot.margin = margin(10, 10, 20, 22, "mm")
    ) +
    labs(
      x = "Event Category",
      y = dependentVarLabel,
      colour = "Group",
      fill   = "Group",
      linetype = "Group",
      caption = sig_legend_text
    ) +
    scale_x_continuous(
      breaks = seq_along(.event_levels),
      labels = .event_labels
    ) +
    scale_colour_manual(values = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_fill_manual(values   = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_linetype_manual(values = c(HC = "solid", BPD = "solid"),
                          breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title)) +
    guides(fill = "none")
  
  if (!is.null(star_df)) {
    star_df$xnum <- as.numeric(star_df$event_category)
    p <- p + geom_text(
      data = star_df,
      aes(x = xnum, y = y, label = stars),
      inherit.aes = FALSE,
      vjust = 0,
      size = 4,
      family = .font_family
    )
  }
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_1_", dv_stub, "_interaction_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_1_", dv_stub, "_interaction_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_colour_manual(values = .group_cols_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_fill_manual(values   = .group_fills_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD"))
    )
  )
  
  p
}

plot_model2_styleA_sjplot <- function(model, dependentVar, output_base_path,
                                      plot_width, plot_height,
                                      x_text_size, y_text_size, y_limits,
                                      figure_label, figure_number, figure_title,
                                      sig_legend_text = .sig_legend_default,
                                      show_stars = FALSE) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  title_suffix <- ""
  if (isTRUE(show_stars)) {
    p_int <- .get_p_m2_interaction(model)
    stars <- .p_to_stars(p_int)
    if (stars != "") title_suffix <- paste0(" ", stars)
  }
  
  p <- sjPlot::plot_model(
    model,
    type  = "int",
    terms = c("sentiment_within", "group"),
    ci.lvl = 0.95
  ) +
    .base_theme(x_text_size = x_text_size, y_text_size = y_text_size) +
    labs(
      x = "Event sentiment (within-person deviation)",
      y = dependentVarLabel,
      colour = "Group",
      fill = "Group",
      caption = sig_legend_text
    ) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title, title_suffix)) +
    scale_color_manual(values = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_fill_manual(values  = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD"))
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleA_sjplot_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleA_sjplot_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_color_manual(values = .group_cols_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_fill_manual(values  = .group_fills_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_shape_manual(values = .group_shapes_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD"))
    )
  )
  
  p
}

plot_model2_styleB_ribbon <- function(model, dependentVar, output_base_path,
                                      plot_width, plot_height,
                                      x_text_size, y_text_size, y_limits,
                                      figure_label, figure_number, figure_title,
                                      sig_legend_text = .sig_legend_default,
                                      show_stars = FALSE) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("sentiment_within [all]", "group")))
  names(pred)[names(pred) == "x"] <- "sentiment_within"
  pred$group <- factor(.norm_group(pred$group), levels = c("HC", "BPD"))
  
  title_suffix <- ""
  if (isTRUE(show_stars)) {
    p_int <- .get_p_m2_interaction(model)
    stars <- .p_to_stars(p_int)
    if (stars != "") title_suffix <- paste0(" ", stars)
  }
  
  p <- ggplot(pred, aes(x = sentiment_within, y = predicted,
                        colour = group, fill = group, linetype = group)) +
    geom_line(linewidth = 0.9) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
    .base_theme(x_text_size = x_text_size, y_text_size = y_text_size) +
    labs(
      x = "Event sentiment (within-person deviation)",
      y = dependentVarLabel,
      colour = "Group",
      fill = "Group",
      linetype = "Group",
      caption = sig_legend_text
    ) +
    scale_colour_manual(values = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_fill_manual(values   = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_linetype_manual(values = c(HC = "solid", BPD = "solid"),
                          breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title, title_suffix)) +
    guides(fill = "none")
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleB_ribbon_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleB_ribbon_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_colour_manual(values = .group_cols_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_fill_manual(values   = .group_fills_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD"))
    )
  )
  
  p
}

plot_model2_styleC_matched <- function(model, dependentVar, output_base_path,
                                       plot_width, plot_height,
                                       x_text_size, y_text_size, y_limits,
                                       figure_label, figure_number, figure_title,
                                       sig_legend_text = .sig_legend_default,
                                       ribbon_alpha = 0.15,
                                       show_stars = FALSE) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  pred_line <- as.data.frame(ggeffects::ggpredict(model, terms = c("sentiment_within [all]", "group")))
  names(pred_line)[names(pred_line) == "x"] <- "sentiment_within"
  pred_line$group <- factor(.norm_group(pred_line$group), levels = c("HC", "BPD"))
  
  x_rng <- range(analysis_df$sentiment_within, na.rm = TRUE)
  x_grid <- seq(x_rng[1], x_rng[2], length.out = 7)
  
  pred_pts <- as.data.frame(
    ggeffects::ggpredict(
      model,
      terms = c(sprintf("sentiment_within [%s]", paste(round(x_grid, 3), collapse = ",")), "group")
    )
  )
  names(pred_pts)[names(pred_pts) == "x"] <- "sentiment_within"
  pred_pts$group <- factor(.norm_group(pred_pts$group), levels = c("HC", "BPD"))
  
  title_suffix <- ""
  if (isTRUE(show_stars)) {
    p_int <- .get_p_m2_interaction(model)
    stars <- .p_to_stars(p_int)
    if (stars != "") title_suffix <- paste0(" ", stars)
  }
  
  p <- ggplot() +
    geom_ribbon(
      data = pred_line,
      aes(x = sentiment_within, ymin = conf.low, ymax = conf.high, fill = group),
      alpha = ribbon_alpha, colour = NA
    ) +
    geom_line(
      data = pred_line,
      aes(x = sentiment_within, y = predicted, colour = group, linetype = group),
      linewidth = 0.9
    ) +
    geom_point(
      data = pred_pts,
      aes(x = sentiment_within, y = predicted, colour = group, shape = group),
      size = 2
    ) +
    .base_theme(x_text_size = x_text_size, y_text_size = y_text_size) +
    labs(
      x = "Event sentiment (within-person deviation)",
      y = dependentVarLabel,
      colour = "Group",
      fill = "Group",
      shape = "Group",
      linetype = "Group",
      caption = sig_legend_text
    ) +
    scale_colour_manual(values = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_fill_manual(values   = .group_cols, breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_shape_manual(values = c(HC = 16, BPD = 16), breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    scale_linetype_manual(values = c(HC = "solid", BPD = "solid"),
                          breaks = c("HC", "BPD"), labels = c("HC", "BPD")) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title, title_suffix)) +
    guides(fill = "none")
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleC_matched_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_styleC_matched_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_colour_manual(values = .group_cols_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_fill_manual(values   = .group_fills_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_shape_manual(values  = .group_shapes_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("HC", "BPD"), labels = c("HC", "BPD"))
    )
  )
  
  p
}


.star_caption <- "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001."

plot_model1_event_x_group(
  model_1_self_esteem, "Self esteem", output_base_path,
  10, 8, 10, 10, c(0, 9),
  figure_label = "Figure", figure_number = "4",
  figure_title = "Model 1 Self-esteem: Interaction between Group and Event Category",
  sig_legend_text = "",
  show_stars = FALSE
)

plot_model1_event_x_group(
  model_1_self_esteem, "Self esteem", output_base_path,
  10, 8, 10, 10, c(0, 9),
  figure_label = "Figure", figure_number = "4",
  figure_title = "Model 1 Self-esteem: Interaction between Group and Event Category",
  sig_legend_text = .star_caption,
  show_stars = TRUE
)

plot_model1_event_x_group(
  model_1_calmness, "Calmness", output_base_path,
  10, 8, 10, 10, c(0, 6),
  figure_label = "Figure", figure_number = "3",
  figure_title = "Model 1 Calmness: Interaction between Group and Event Category",
  sig_legend_text = .star_caption,
  show_stars = TRUE
)


plot_model2_styleC_matched(
  model_2_self_esteem, "Self esteem", output_base_path,
  10, 8, 10, 10, c(0, 9),
  figure_label = "Figure", figure_number = "6",
  figure_title = "Model 2 Self-esteem: Interaction between Group and Sentiment (Within-person)",
  sig_legend_text = .star_caption,
  show_stars = TRUE
)

plot_model2_styleC_matched(
  model_2_valence, "Valence", output_base_path,
  10, 8, 10, 10, c(0, 6),
  figure_label = "Figure", figure_number = "5",
  figure_title = "Model 2 Valence: Interaction between Group and Sentiment (Within-person)",
  sig_legend_text = .star_caption,
  show_stars = TRUE
)

plot_model2_styleC_matched(
  model_2_calmness, "Calmness", output_base_path,
  10, 8, 10, 10, c(0, 6),
  figure_label = "Figure", figure_number = "7",
  figure_title = "Model 2 Calmness: Interaction between Group and Sentiment (Within-person)",
  sig_legend_text = .star_caption,
  show_stars = TRUE
)

plot_model2_styleA_sjplot(
  model_2_self_esteem, "Self esteem", output_base_path,
  10, 8, 10, 10, c(0, 9),
  figure_label = "Figure", figure_number = "4A",
  figure_title = "Model 2 Self-esteem: Interaction between Group and Sentiment (Within-person) (p < .001)",
  sig_legend_text = "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001."
)

plot_model2_styleB_ribbon(
  model_2_self_esteem, "Self esteem", output_base_path,
  10, 8, 10, 10, c(0, 9),
  figure_label = "Figure", figure_number = "4B",
  figure_title = "Model 2 Self-esteem: Interaction between Group and Sentiment (Within-person) (p < .001)",
  sig_legend_text = ""
)

plot_model2_styleA_sjplot(
  model_2_valence, "Valence", output_base_path,
  10, 8, 10, 10, c(0, 6),
  figure_label = "Figure", figure_number = "3A",
  figure_title = "Model 2 Valence: Interaction between Group and Within-person Sentiment (p < .001)",
  sig_legend_text = "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001."
)

plot_model2_styleB_ribbon(
  model_2_valence, "Valence", output_base_path,
  10, 8, 10, 10, c(0, 6),
  figure_label = "Figure", figure_number = "3B",
  figure_title = "Model 2 Valence: Interaction between Group and Within-person Sentiment (p < .001)",
  sig_legend_text = "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001."
)

output_bsl_path <- "<bsl output path>"

if (!dir.exists(output_bsl_path)) {
  dir.create(output_bsl_path, recursive = TRUE)
}


print("M1C and M2C -BSL-SCORE (ALL)")

all_df_bsl <- analysis_df

cat("Unique ids in all_df_bsl:", length(unique(all_df_bsl$id)), "\n")
cat("Non-missing BSL_score (ALL):", sum(!is.na(all_df_bsl$BSL_score)), "\n")
cat("Non-missing BSL_score by group:\n")
print(tapply(!is.na(all_df_bsl$BSL_score), all_df_bsl$group, sum))


model_1c_self_esteem_bsl <- create_and_summarize_save_model(
  "self_esteem", "~ event_category + group + BSL_score + event_category:BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 1
)

model_1c_valence_bsl <- create_and_summarize_save_model(
  "valence", "~ event_category + group + BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 1
)

model_1c_calmness_bsl <- create_and_summarize_save_model(
  "calmness", "~ event_category + group + BSL_score + event_category:BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 1
)

print("M1C-END")



model_2c_self_esteem_bsl <- create_and_summarize_save_model(
  "self_esteem", "~ event_category + group + sentiment_within + sentiment_mean + BSL_score + sentiment_within * BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 2
)

model_2c_valence_bsl <- create_and_summarize_save_model(
  "valence", "~ event_category + group + sentiment_within + sentiment_mean + BSL_score + sentiment_within * BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 2
)

model_2c_calmness_bsl <- create_and_summarize_save_model(
  "calmness", "~ event_category + group + sentiment_within + sentiment_mean + BSL_score + sentiment_within * BSL_score + (1 | id)",
  all_df_bsl, output_bsl_path, 2
)

print("M2C-END")

print("M1B and M2B -BSL-SCORE (BPD ONLY)")

bpd_df_bsl <- analysis_df %>%
  dplyr::filter(group == "1")

cat("Unique BPD ids in bpd_df_bsl:", length(unique(bpd_df_bsl$id)), "\n")
cat("Non-missing BSL_score in BPD:", sum(!is.na(bpd_df_bsl$BSL_score)), "\n")

model_1b_self_esteem_bsl <- create_and_summarize_save_model_bpd(
  "self_esteem", "~ event_category + BSL_score + (1 | id)",
  bpd_df_bsl, output_bsl_path, 111
)
model_1b_valence_bsl <- create_and_summarize_save_model_bpd(
  "valence", "~ event_category + BSL_score + (1 | id)",
  bpd_df_bsl, output_bsl_path, 111
)
model_1b_calmness_bsl <- create_and_summarize_save_model_bpd(
  "calmness", "~ event_category + BSL_score + (1 | id)",
  bpd_df_bsl, output_bsl_path, 111
)


model_2b_self_esteem_bsl <- create_and_summarize_save_model_bpd(
  "self_esteem", "~ event_category + sentiment_within + BSL_score + sentiment_mean + (1 | id)",
  bpd_df_bsl, output_bsl_path, 222
)
model_2b_valence_bsl <- create_and_summarize_save_model_bpd(
  "valence", "~ event_category + sentiment_within + BSL_score + sentiment_mean + (1 | id)",
  bpd_df_bsl, output_bsl_path, 222
)
model_2b_calmness_bsl <- create_and_summarize_save_model_bpd(
  "calmness", "~ event_category + sentiment_within + BSL_score + sentiment_mean + (1 | id)",
  bpd_df_bsl, output_bsl_path, 222
)

print("M2B-END")


output_covariate_path <- file.path(
  "<covariate output path>",
  ""
)

if (!dir.exists(output_covariate_path)) {
  dir.create(output_covariate_path, recursive = TRUE)
}

print(paste0("Saving covariate model outputs to: ", output_covariate_path))

print("M1-COV")

model_1_self_esteem_cov <- create_and_summarize_save_model(
  "self_esteem", "~ event_category + group + family_status + highest_education + work_status + event_category:group + (1 | id)",
  analysis_df, output_covariate_path, 11
)
model_1_valence_cov <- create_and_summarize_save_model(
  "valence", "~ event_category + group + family_status + highest_education + work_status + event_category:group + (1 | id)",
  analysis_df, output_covariate_path, 11
)
model_1_calmness_cov <- create_and_summarize_save_model(
  "calmness","~ event_category + group + family_status + highest_education + work_status + event_category:group + (1 | id)",
  analysis_df, output_covariate_path, 11
)

print("M1-COV-END")

print("M2-COV")

model_2_self_esteem_cov <- create_and_summarize_save_model(
  "self_esteem",
  "~ event_category +
     sentiment_within * group +
     sentiment_mean + family_status + highest_education + work_status +
     (1 | id)",
  analysis_df, output_covariate_path, 12
)

model_2_valence_cov <- create_and_summarize_save_model(
  "valence",
  "~ event_category +
     sentiment_within * group +
     sentiment_mean + family_status + highest_education + work_status +
     (1 | id)",
  analysis_df, output_covariate_path, 12
)

model_2_calmness_cov <- create_and_summarize_save_model(
  "calmness",
  "~ event_category +
     sentiment_within * group +
     sentiment_mean +
     group + family_status + highest_education + work_status +
     (1 | id)",
  analysis_df, output_covariate_path, 12
)

print("M2-COV-END")



print("Bootstrap Models Function")
set.seed(123)  

create_and_summarize_bootstrap <- function(dependent_var, formula_part, data, output_base_path, rq_number, R = 10000) {
  data$group <- factor(data$group, levels = c("1", "2"), labels = c("BPD", "HC"))
  data$group <- relevel(data$group, ref = "HC")
  
  formula <- as.formula(paste(dependent_var, formula_part))
  
  initial_model <- tryCatch({
    lmerTest::lmer(formula, data = data, REML = FALSE)
  }, error = function(e) {
    cat("Error: Initial model fitting failed with message: ", e$message, "\n")
    return(NULL)  
  })
  
  if (is.null(initial_model)) {
    cat("Initial model fitting unsuccessful. Exiting function.\n")
    return(NULL)
  }
  
  num_fixed_effects <- length(fixef(initial_model))
  boot_coefs <- matrix(NA, nrow = R, ncol = num_fixed_effects)
  colnames(boot_coefs) <- names(fixef(initial_model))
  
  unique_ids <- unique(data$id)
  
  n_success <- 0L
  for (i in seq_len(R)) {
    sampled_ids <- sample(unique_ids, replace = TRUE)
    boot_data <- do.call(
      rbind,
      lapply(sampled_ids, function(s) data[data$id == s, , drop = FALSE])
    )
    boot_model <- tryCatch(
      lmerTest::lmer(formula, data = boot_data, REML = FALSE),
      error = function(e) NULL
    )
    if (!is.null(boot_model) && length(fixef(boot_model)) == num_fixed_effects) {
      n_success <- n_success + 1L
      boot_coefs[n_success, ] <- fixef(boot_model)  
    }
  }
  if (n_success < R) {
    boot_coefs <- boot_coefs[seq_len(n_success), , drop = FALSE]
  }
  message(sprintf("Bootstrap fits succeeded: %d / %d (%.1f%%)", n_success, R, 100 * n_success / R))
  
  boot_cis <- apply(boot_coefs, 2, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))
  
  bootstrap_results <- data.frame(
    Mean     = apply(boot_coefs, 2, mean, na.rm = TRUE),
    CI_Lower = boot_cis["2.5%", ],
    CI_Upper = boot_cis["97.5%", ]
  )
  
  bootstrap_results_file <- data.frame(Coefficient = colnames(boot_coefs), bootstrap_results)
  write.csv(
    bootstrap_results_file,
    paste0(output_base_path, "bootstrap_results_RQ", rq_number, "_", dependent_var, ".csv"),
    row.names = FALSE
  )
  
  return(bootstrap_results_file)
}

create_and_summarize_bootstrap_bpd <- function(dependent_var, formula_part, data, output_base_path, rq_number, R = 10000) {
  
  formula <- as.formula(paste(dependent_var, formula_part))
  
  initial_model <- tryCatch({
    lmerTest::lmer(formula, data = data, REML = FALSE)
  }, error = function(e) {
    cat("Error: Initial BPD-only model fitting failed with message: ", e$message, "\n")
    return(NULL)  
  })
  
  if (is.null(initial_model)) {
    cat("Initial BPD-only model fitting unsuccessful. Exiting function.\n")
    return(NULL)
  }
  
  num_fixed_effects <- length(fixef(initial_model))
  boot_coefs <- matrix(NA, nrow = R, ncol = num_fixed_effects)
  colnames(boot_coefs) <- names(fixef(initial_model))
  
  unique_ids <- unique(data$id)
  
  n_success <- 0L
  for (i in seq_len(R)) {
    sampled_ids <- sample(unique_ids, replace = TRUE)
    boot_data <- do.call(
      rbind,
      lapply(sampled_ids, function(s) data[data$id == s, , drop = FALSE])
    )
    boot_model <- tryCatch(
      lmerTest::lmer(formula, data = boot_data, REML = FALSE),
      error = function(e) NULL
    )
    if (!is.null(boot_model) && length(fixef(boot_model)) == num_fixed_effects) {
      n_success <- n_success + 1L
      boot_coefs[n_success, ] <- fixef(boot_model)
    }
  }
  if (n_success < R) {
    boot_coefs <- boot_coefs[seq_len(n_success), , drop = FALSE]
  }
  message(sprintf("[BPD-only] Bootstrap fits succeeded: %d / %d (%.1f%%)", n_success, R, 100 * n_success / R))
  
  boot_cis <- apply(boot_coefs, 2, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))
  
  bootstrap_results <- data.frame(
    Mean     = apply(boot_coefs, 2, mean, na.rm = TRUE),
    CI_Lower = boot_cis["2.5%", ],
    CI_Upper = boot_cis["97.5%", ]
  )
  
  bootstrap_results_file <- data.frame(Coefficient = colnames(boot_coefs), bootstrap_results)
  write.csv(
    bootstrap_results_file,
    paste0(output_base_path, "bootstrap_results_RQ", rq_number, "_", dependent_var, ".csv"),
    row.names = FALSE
  )
  
  return(bootstrap_results_file)
}
print("Bootstrap Models Function END")


print("BM1")

bootstrap_model_1_self_esteem <- create_and_summarize_bootstrap(
  "self_esteem", "~ event_category + group + event_category:group + (1 | id)",
  analysis_df, output_base_path, 1, R = 10000
)
bootstrap_model_1_valence <- create_and_summarize_bootstrap(
  "valence", "~ event_category + group + (1 | id)",
  analysis_df, output_base_path, 1, R = 10000
)
bootstrap_model_1_calmness <- create_and_summarize_bootstrap(
  "calmness", "~ event_category + group + event_category:group + (1 | id)",
  analysis_df, output_base_path, 1, R = 10000
)

print("BM1-END")

print("BM2")

bootstrap_model_2_self_esteem <- create_and_summarize_bootstrap(
  "self_esteem", "~ event_category + sentiment_within * group + sentiment_mean + group + (1 | id)",
  analysis_df, output_base_path, 2, R = 10000
)

bootstrap_model_2_valence <- create_and_summarize_bootstrap(
  "valence", "~ event_category + sentiment_within * group + sentiment_mean + group + (1 | id)",
  analysis_df, output_base_path, 2, R = 10000
)

bootstrap_model_2_calmness <- create_and_summarize_bootstrap(
  "calmness", "~ event_category + sentiment_within * group + sentiment_mean + group + (1 | id)",
  analysis_df, output_base_path, 2, R = 10000
)

print("BM2-END")

print("BM3")

bootstrap_model_3_self_esteem <- create_and_summarize_bootstrap(
  "self_esteem",
  "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3, R = 10000
)
bootstrap_model_3_valence <- create_and_summarize_bootstrap(
  "valence",
  "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3, R = 10000
)
bootstrap_model_3_calmness <- create_and_summarize_bootstrap(
  "calmness",
  "~ event_subcategory + group + (1 | id)",
  analysis_df, output_base_path, 3, R = 10000
)

print("BM3-END")


calculate_r2_df <- function(model, model_name) {
  r2_values <- r.squaredGLMM(model)
  data.frame(
    Model          = model_name,
    R2_Marginal    = r2_values[1, "R2m"],
    R2_Conditional = r2_values[1, "R2c"]
  )
}

models <- list(
  M1_self_esteem        = model_1_self_esteem,
  M1_valence            = model_1_valence,
  M1_calmness           = model_1_calmness,
  
  M2_self_esteem        = model_2_self_esteem,
  M2_valence            = model_2_valence,
  M2_calmness           = model_2_calmness,
  
  M3_self_esteem        = model_3_self_esteem,
  M3_valence            = model_3_valence,
  M3_calmness           = model_3_calmness,
  
  M1B_self_esteem_BPD   = model_1b_self_esteem_bsl,
  M1B_valence_BPD       = model_1b_valence_bsl,
  M1B_calmness_BPD      = model_1b_calmness_bsl,
  
  M2B_self_esteem_BPD   = model_2b_self_esteem_bsl,
  M2B_valence_BPD       = model_2b_valence_bsl,
  M2B_calmness_BPD      = model_2b_calmness_bsl,
  
  M1_self_esteem_COV    = model_1_self_esteem_cov,
  M1_valence_COV        = model_1_valence_cov,
  M1_calmness_COV       = model_1_calmness_cov,
  
  M2_self_esteem_COV    = model_2_self_esteem_cov,
  M2_valence_COV        = model_2_valence_cov,
  M2_calmness_COV       = model_2_calmness_cov
)

results_df <- do.call(
  rbind,
  lapply(names(models), function(name) {
    calculate_r2_df(models[[name]], name)
  })
)

tryCatch({
  write.csv2(
    results_df,
    file = paste0(output_base_path, "Combined_R2_Results_all_models.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}, error = function(e) {
  print(paste("Failed to write CSV file:", e$message))
})

model_1_self_esteem
model_1_valence
model_1_calmness

model_2_self_esteem
model_2_valence
model_2_calmness

model_3_self_esteem
model_3_valence
model_3_calmness

model_1b_self_esteem_bsl
model_1b_valence_bsl
model_1b_calmness_bsl

model_2b_self_esteem_bsl
model_2b_valence_bsl
model_2b_calmness_bsl

model_1_self_esteem_cov
model_1_valence_cov
model_1_calmness_cov

model_2_self_esteem_cov
model_2_valence_cov
model_2_calmness_cov

sink(type = "output")
sink(type = "message")
close(log_conn)


install_and_load_package("emmeans")
install_and_load_package("ggplot2")
install_and_load_package("ggeffects")
install_and_load_package("dplyr")

.font_family <- "Arial"
.base_size   <- 12

.pal_colour <- c(HC = "
.pal_bw     <- c(HC = "
.lty_bw     <- c(HC = "solid",   BPD = "dashed")
.shape_bw   <- c(HC = 16,        BPD = 17)

.event_levels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate"
)

.event_labels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate Event"
)

theme_modern <- function() {
  ggplot2::theme_minimal(base_family = .font_family, base_size = .base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(linewidth = 0.6, colour = "
      panel.grid.minor = ggplot2::element_line(linewidth = 0.4, colour = "
      axis.line        = ggplot2::element_line(linewidth = 0.8, colour = "
      axis.ticks       = ggplot2::element_line(linewidth = 0.7, colour = "
      plot.title       = ggplot2::element_text(face = "bold"),
      legend.position  = "right",
      legend.title     = ggplot2::element_text(face = "bold"),
      plot.margin      = ggplot2::margin(10, 10, 10, 10)
    )
}

.p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

.save_colour_bw <- function(p_colour, basefile, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(paste0(basefile, "_colour.png"), p_colour, width = width, height = height, dpi = dpi)
  
  p_bw <- p_colour +
    ggplot2::scale_colour_manual(values = .pal_bw) +
    ggplot2::scale_fill_manual(values   = .pal_bw) +
    ggplot2::scale_linetype_manual(values = .lty_bw) +
    ggplot2::scale_shape_manual(values   = .shape_bw)
  
  ggplot2::ggsave(paste0(basefile, "_bw.png"), p_bw, width = width, height = height, dpi = dpi)
  
  invisible(list(colour = p_colour, bw = p_bw))
}

get_m1_simpleeffect_stars <- function(model) {
  em <- emmeans::emmeans(model, ~ group | event_category)
  ct <- as.data.frame(emmeans::contrast(em, method = "revpairwise"))  
  out <- ct %>%
    dplyr::mutate(
      event_category = as.character(event_category),
      stars = vapply(p.value, .p_to_stars, character(1))
    ) %>%
    dplyr::select(event_category, stars) %>%
    dplyr::filter(stars != "")
  if (nrow(out) == 0) return(NULL)
  out$event_category <- factor(out$event_category, levels = .event_levels)
  out
}

plot_m1_event_by_group <- function(model, ylab, y_limits, out_base,
                                   title = NULL, show_simpleeffect_stars = TRUE) {
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("event_category", "group")))
  names(pred)[names(pred) == "x"] <- "event_category"
  pred$event_category <- factor(as.character(pred$event_category), levels = .event_levels)
  pred$group <- factor(as.character(pred$group), levels = c("HC", "BPD"))
  
  star_df <- NULL
  if (isTRUE(show_simpleeffect_stars)) {
    star_df <- get_m1_simpleeffect_stars(model)
    if (!is.null(star_df)) {
      star_df$y <- y_limits[2] - 0.04 * diff(y_limits)
    }
  }
  
  p <- ggplot2::ggplot(
    pred,
    ggplot2::aes(x = event_category, y = predicted, colour = group,
                 group = group, linetype = group, shape = group)
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                           width = 0.12, linewidth = 0.7) +
    ggplot2::scale_x_discrete(limits = .event_levels, labels = .event_labels) +
    ggplot2::scale_colour_manual(values = .pal_colour) +
    ggplot2::scale_linetype_manual(values = c(HC = "solid", BPD = "solid")) +
    ggplot2::scale_shape_manual(values = c(HC = 16, BPD = 16)) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(
      title = title,
      x = "Event category",
      y = ylab,
      colour = "Group",
      linetype = "Group",
      shape = "Group"
    ) +
    theme_modern() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  
  if (!is.null(star_df)) {
    p <- p + ggplot2::geom_text(
      data = star_df,
      ggplot2::aes(x = event_category, y = y, label = stars),
      inherit.aes = FALSE, vjust = 0, size = 4
    )
  }
  
  .save_colour_bw(p, out_base, width = 11.2, height = 5.6, dpi = 300)
  p
}

get_m2_interaction_p <- function(model) {
  ct <- as.data.frame(summary(model)$coefficients)
  ct$term <- rownames(ct)
  idx <- which(grepl("sentiment_within", ct$term) & grepl("groupBPD", ct$term))
  if (length(idx) == 0) return(NA_real_)
  if (!"Pr(>|t|)" %in% names(ct)) return(NA_real_)
  as.numeric(ct$`Pr(>|t|)`[idx[1]])
}

plot_m2_sentiment_by_group <- function(model, yvar, ylab, y_limits, out_base,
                                       title = NULL, add_stars_to_title = TRUE) {
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("sentiment_within [all]", "group")))
  names(pred)[names(pred) == "x"] <- "sentiment_within"
  pred$group <- factor(as.character(pred$group), levels = c("HC", "BPD"))
  
  mf <- model.frame(model)
  raw <- mf %>%
    dplyr::select(group, sentiment_within, !!rlang::sym(yvar)) %>%
    dplyr::rename(y = !!rlang::sym(yvar)) %>%
    dplyr::mutate(group = factor(as.character(group), levels = c("HC", "BPD"))) %>%
    dplyr::filter(!is.na(sentiment_within), !is.na(y))
  
  p_int <- get_m2_interaction_p(model)
  stars <- .p_to_stars(p_int)
  ttl <- title
  if (isTRUE(add_stars_to_title) && !is.null(ttl) && nzchar(stars)) {
    ttl <- paste0(ttl, " ", stars)
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = raw,
      ggplot2::aes(x = sentiment_within, y = y, colour = group, shape = group),
      alpha = 0.08, size = 1.6
    ) +
    ggplot2::geom_ribbon(
      data = pred,
      ggplot2::aes(x = sentiment_within, ymin = conf.low, ymax = conf.high, fill = group),
      alpha = 0.18, colour = NA
    ) +
    ggplot2::geom_line(
      data = pred,
      ggplot2::aes(x = sentiment_within, y = predicted, colour = group, linetype = group),
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.9, alpha = 0.6) +
    ggplot2::scale_colour_manual(values = .pal_colour) +
    ggplot2::scale_fill_manual(values = .pal_colour) +
    ggplot2::scale_linetype_manual(values = c(HC = "solid", BPD = "solid")) +
    ggplot2::scale_shape_manual(values = c(HC = 16, BPD = 16)) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(
      title = ttl,
      x = "Event sentiment (within-person deviation)",
      y = ylab,
      colour = "Group",
      fill = "Group",
      linetype = "Group",
      shape = "Group"
    ) +
    theme_modern()
  
  .save_colour_bw(p, out_base, width = 9.2, height = 5.2, dpi = 300)
  p
}


plot_m1_event_by_group(
  model_1_self_esteem,
  ylab = "Self-esteem",
  y_limits = c(0, 9),
  out_base = file.path(output_base_path, "Figure_4_model_1_self_esteem_event_x_group"),
  title = "Model 1: Self-esteem by event category and group",
  show_simpleeffect_stars = TRUE
)

plot_m1_event_by_group(
  model_1_calmness,
  ylab = "Calmness",
  y_limits = c(0, 6),
  out_base = file.path(output_base_path, "Figure_3_model_1_calmness_event_x_group"),
  title = "Model 1: Calmness by event category and group",
  show_simpleeffect_stars = TRUE
)

plot_m2_sentiment_by_group(
  model_2_self_esteem,
  yvar = "self_esteem",
  ylab = "Self-esteem",
  y_limits = c(0, 9),
  out_base = file.path(output_base_path, "Figure_6_model_2_self_esteem_sentiment_within_x_group"),
  title = "Model 2: Self-esteem reactivity to within-person sentiment",
  add_stars_to_title = TRUE
)

plot_m2_sentiment_by_group(
  model_2_valence,
  yvar = "valence",
  ylab = "Valence",
  y_limits = c(0, 6),
  out_base = file.path(output_base_path, "Figure_5_model_2_valence_sentiment_within_x_group"),
  title = "Model 2: Valence reactivity to within-person sentiment",
  add_stars_to_title = TRUE
)

plot_m2_sentiment_by_group(
  model_2_calmness,
  yvar = "calmness",
  ylab = "Calmness",
  y_limits = c(0, 6),
  out_base = file.path(output_base_path, "Figure_7_model_2_calmness_sentiment_within_x_group"),
  title = "Model 2: Calmness reactivity to within-person sentiment",
  add_stars_to_title = TRUE
)


library(ggplot2)
library(dplyr)
library(ggeffects)  
library(ggtext)     
library(htmltools)  
library(emmeans)    

.font_family <- "Times New Roman"
.font_size   <- 12

.group_cols <- c(HC = "

.event_levels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate"
)

.event_labels <- c(
  "Daily Routines and Household Activities",
  "Leisure and Recreation",
  "Social and Personal Relationships",
  "Work and Professional Engagements",
  "Health and Well-being",
  "Indeterminate Event"
)

.sig_legend_default <- "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001."

.base_theme <- function(x_text_size = 10, y_text_size = 10) {
  theme_classic() +
    theme(
      text        = element_text(family = .font_family, size = .font_size),
      axis.title  = element_text(family = .font_family, size = .font_size),
      axis.text.x = element_text(family = .font_family, size = x_text_size),
      axis.text.y = element_text(family = .font_family, size = y_text_size),
      legend.title = element_text(family = .font_family, size = .font_size),
      legend.text  = element_text(family = .font_family, size = .font_size),
      legend.key   = element_blank(),
      plot.title = ggtext::element_markdown(hjust = 0, family = .font_family, size = .font_size),
      plot.caption = element_text(
        family = .font_family,
        size   = .font_size,
        hjust  = 0,
        margin = margin(t = 10)
      ),
      plot.margin = margin(10, 10, 10, 10, "mm")
    )
}

.group_cols_bw   <- c(HC = "
.group_ltys_bw   <- c(HC = "solid",   BPD = "dashed")
.group_shapes_bw <- c(HC = 16,        BPD = 17)
.group_fills_bw  <- c(HC = "

.save_plot_two_versions <- function(p, filename_colour, filename_bw,
                                    width, height, dpi = 300,
                                    bw_scales = NULL) {
  ggsave(filename_colour, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  
  p_bw <- p
  if (!is.null(bw_scales)) {
    for (sc in bw_scales) p_bw <- p_bw + sc
  }
  ggsave(filename_bw, plot = p_bw, width = width, height = height, dpi = dpi, bg = "white")
  
  invisible(list(colour = p, bw = p_bw))
}

.p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

.norm_group <- function(x) {
  x <- as.character(x)
  x[x == "1"] <- "BPD"
  x[x == "2"] <- "HC"
  x
}

.get_p_m2_interaction <- function(model) {
  ct <- as.data.frame(summary(model)$coefficients)
  ct$term <- rownames(ct)
  idx <- which(grepl("sentiment_within", ct$term) & grepl("groupBPD", ct$term))
  if (length(idx) >= 1 && "Pr(>|t|)" %in% colnames(ct)) return(as.numeric(ct$`Pr(>|t|)`[idx[1]]))
  NA_real_
}

.get_m1_simpleeffect_stars_df <- function(model, y_limits) {
  em <- emmeans::emmeans(model, ~ group | event_category)
  ct <- as.data.frame(emmeans::contrast(em, method = "revpairwise"))  
  
  if (!"p.value" %in% names(ct)) return(NULL)
  
  star_df <- ct %>%
    dplyr::mutate(
      event_category = as.character(event_category),
      stars = vapply(p.value, .p_to_stars, character(1))
    ) %>%
    dplyr::filter(stars != "") %>%
    dplyr::select(event_category, stars)
  
  if (nrow(star_df) == 0) return(NULL)
  
  star_df$event_category <- factor(star_df$event_category, levels = .event_levels)
  star_df$y <- y_limits[2] - 0.04 * diff(y_limits)
  star_df
}

.make_title_html <- function(figure_label, figure_number, figure_title, title_suffix = "") {
  HTML(paste0(
    "<span style='font-family:", .font_family, "; font-size:12pt;'><strong>",
    figure_label, " ", figure_number,
    "</strong></span><br><br>",
    "<span style='font-family:", .font_family, "; font-size:12pt;'><em>",
    figure_title, title_suffix,
    "</em></span>"
  ))
}

.file_token <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

.figure_file_stub <- function(figure_label, figure_number) {
  paste(.file_token(figure_label), .file_token(figure_number), sep = "_")
}

.add_panel_label <- function(p, label = NULL) {
  if (is.null(label) || !nzchar(label)) return(p)
  p + annotate(
    "text",
    x = -Inf, y = Inf,
    label = label,
    hjust = -0.2, vjust = 1.2,
    family = .font_family, fontface = "bold", size = 5
  )
}

plot_model1_event_x_group <- function(model, dependentVar, output_base_path,
                                      plot_width, plot_height,
                                      x_text_size, y_text_size, y_limits,
                                      figure_label, figure_number, figure_title,
                                      sig_legend_text = .sig_legend_default,
                                      panel_label = NULL,
                                      point_alpha = 0.18,
                                      point_size  = 1.6,
                                      stars = TRUE) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  dv_name <- all.vars(formula(model))[1]
  
  raw <- model.frame(model) %>%
    dplyr::transmute(
      event_category = factor(as.character(event_category), levels = .event_levels),
      group = factor(.norm_group(group), levels = c("HC", "BPD")),
      y = .data[[dv_name]]
    ) %>%
    dplyr::filter(is.finite(y))
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("event_category", "group")))
  names(pred)[names(pred) == "x"] <- "event_category"
  pred$group <- factor(.norm_group(pred$group), levels = c("HC", "BPD"))
  pred$event_category <- factor(as.character(pred$event_category), levels = .event_levels)
  
  star_df <- NULL
  if (isTRUE(stars)) star_df <- .get_m1_simpleeffect_stars_df(model, y_limits)
  
  p <- ggplot() +
    geom_point(
      data = raw,
      aes(x = event_category, y = y, colour = group, shape = group),
      position = position_jitter(width = 0.12, height = 0),
      alpha = point_alpha, size = point_size
    ) +
    geom_line(
      data = pred,
      aes(x = event_category, y = predicted, colour = group, group = group, linetype = group),
      linewidth = 0.95
    ) +
    geom_ribbon(
      data = pred,
      aes(x = event_category, ymin = conf.low, ymax = conf.high, fill = group, group = group),
      alpha = 0.18, colour = NA
    ) +
    geom_point(
      data = pred,
      aes(x = event_category, y = predicted, colour = group),
      size = 2.4
    ) +
    .base_theme(x_text_size = x_text_size, y_text_size = y_text_size) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
      plot.margin = margin(10, 10, 20, 22, "mm")
    ) +
    labs(
      x = "Event Category",
      y = dependentVarLabel,
      colour = "Group",
      fill   = "Group",
      linetype = "Group",
      shape = "Group",
      caption = sig_legend_text
    ) +
    scale_x_discrete(limits = .event_levels, labels = .event_labels) +
    scale_colour_manual(values = .group_cols, breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    scale_fill_manual(values   = .group_cols, breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    scale_shape_manual(values = c(HC = 16, BPD = 16), breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    scale_linetype_manual(values = c(HC = "solid", BPD = "solid"), breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title)) +
    guides(
      colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 4)),
      fill   = "none",
      shape  = "none"
    )
  
  if (!is.null(star_df)) {
    p <- p + geom_text(
      data = star_df,
      aes(x = event_category, y = y, label = stars),
      inherit.aes = FALSE,
      vjust = 0,
      size = 4,
      family = .font_family
    )
  }
  
  p <- .add_panel_label(p, panel_label)
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_1_", dv_stub, "_interaction_example_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_1_", dv_stub, "_interaction_example_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_colour_manual(values = .group_cols_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC")),
      scale_fill_manual(values   = .group_fills_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC")),
      scale_shape_manual(values   = .group_shapes_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC"))
    )
  )
  
  p
}

plot_model2_sentiment_x_group <- function(model, dependentVar, output_base_path,
                                          plot_width, plot_height,
                                          y_limits,
                                          figure_label, figure_number, figure_title,
                                          sig_legend_text = .sig_legend_default,
                                          panel_label = NULL,
                                          point_alpha = 0.18,
                                          point_size  = 1.7,
                                          ribbon_alpha = 0.22) {
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  dv_name <- all.vars(formula(model))[1]
  
  raw <- model.frame(model) %>%
    dplyr::transmute(
      group = factor(.norm_group(group), levels = c("HC", "BPD")),
      sentiment_within = sentiment_within,
      y = .data[[dv_name]]
    ) %>%
    dplyr::filter(is.finite(sentiment_within), is.finite(y))
  
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = c("sentiment_within [all]", "group")))
  names(pred)[names(pred) == "x"] <- "sentiment_within"
  pred$group <- factor(.norm_group(pred$group), levels = c("HC", "BPD"))
  
  p_int <- .get_p_m2_interaction(model)
  stars <- .p_to_stars(p_int)
  title_suffix <- if (stars != "") paste0(" ", stars) else ""
  
  p <- ggplot() +
    geom_point(
      data = raw,
      aes(x = sentiment_within, y = y, colour = group),
      alpha = point_alpha, size = point_size
    ) +
    geom_ribbon(
      data = pred,
      aes(x = sentiment_within, ymin = conf.low, ymax = conf.high, fill = group),
      alpha = ribbon_alpha, colour = NA
    ) +
    geom_line(
      data = pred,
      aes(x = sentiment_within, y = predicted, colour = group, linetype = group),
      linewidth = 1.05
    ) +
    .base_theme(x_text_size = 10, y_text_size = 10) +
    labs(
      x = "Event sentiment (within-person deviation)",
      y = dependentVarLabel,
      colour = "Group",
      fill   = "Group",
      linetype = "Group",
      caption = sig_legend_text
    ) +
    scale_colour_manual(values = .group_cols, breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    scale_fill_manual(values   = .group_cols, breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    scale_linetype_manual(values = c(HC = "solid", BPD = "solid"), breaks = c("BPD", "HC"), labels = c("BPD", "HC")) +
    coord_cartesian(ylim = y_limits) +
    ggtitle(.make_title_html(figure_label, figure_number, figure_title, title_suffix)) +
    guides(
      colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 4)),
      fill   = "none"
    )
  
  p <- .add_panel_label(p, panel_label)
  
  file_stub <- .figure_file_stub(figure_label, figure_number)
  dv_stub <- .file_token(dependentVar)
  file_colour <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_example_colour.png"))
  file_bw     <- file.path(output_base_path, paste0(file_stub, "_model_2_", dv_stub, "_interaction_example_bw.png"))
  
  .save_plot_two_versions(
    p,
    filename_colour = file_colour,
    filename_bw     = file_bw,
    width = plot_width, height = plot_height, dpi = 300,
    bw_scales = list(
      scale_colour_manual(values = .group_cols_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC")),
      scale_fill_manual(values   = .group_fills_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC")),
      scale_linetype_manual(values = .group_ltys_bw, breaks = c("BPD", "HC"), labels = c("BPD", "HC"))
    )
  )
  
  p
}


plot_model1_event_x_group(
  model_1_self_esteem, "Self esteem", output_base_path,
  plot_width = 10, plot_height = 8,
  x_text_size = 10, y_text_size = 10,
  y_limits = c(0, 9),
  figure_label = "Figure", figure_number = "4",
  figure_title = "Model 1 Self-esteem: Group × Event Category",
  sig_legend_text = "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001.",
  panel_label = NULL,   
  stars = TRUE
)

plot_model1_event_x_group(
  model_1_calmness, "Calmness", output_base_path,
  plot_width = 10, plot_height = 8,
  x_text_size = 10, y_text_size = 10,
  y_limits = c(0, 6),
  figure_label = "Figure", figure_number = "3",
  figure_title = "Model 1 Calmness: Group × Event Category",
  sig_legend_text = "* indicates p < .05, ** indicates p < .01, and *** indicates p < .001.",
  panel_label = NULL,
  stars = TRUE
)


plot_model2_sentiment_x_group(
  model_2_self_esteem, "Self esteem", output_base_path,
  plot_width = 10, plot_height = 8,
  y_limits = c(0, 9),
  figure_label = "Figure", figure_number = "6",
  figure_title = "Model 2 Self-esteem: Group × Within-person Sentiment",
  sig_legend_text = "",  
  panel_label = NULL
)

plot_model2_sentiment_x_group(
  model_2_valence, "Valence", output_base_path,
  plot_width = 10, plot_height = 8,
  y_limits = c(0, 6),
  figure_label = "Figure", figure_number = "5",
  figure_title = "Model 2 Valence: Group × Within-person Sentiment",
  sig_legend_text = "",
  panel_label = NULL
)

plot_model2_sentiment_x_group(
  model_2_calmness, "Calmness", output_base_path,
  plot_width = 10, plot_height = 8,
  y_limits = c(0, 6),
  figure_label = "Figure", figure_number = "7",
  figure_title = "Model 2 Calmness: Group × Within-person Sentiment",
  sig_legend_text = "",
  panel_label = NULL
)
