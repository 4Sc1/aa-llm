
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


# --------------------- config ---------------------
# logging
log_conn <- file("<path to logfile>", open = "wt")
sink(log_conn, type = "output")
sink(log_conn, type = "message")

# packages
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

required_packages <- c("dplyr", "lme4", "pROC", "ggplot2", "car", "knitr", "ggeffects", "viridis", "lmerTest", "ggdist", "ggpubr", "lattice", "lubridate", "ggpredict", "papaja", "grid", "gridExtra", "magrittr", "forecast", "sjPlot", "webshot", "MuMIn")

invisible(lapply(required_packages, install_and_load_package))

# ----------- loading and pre-processing -----------
file_path <- "<path to input CSV file>"
setwd(dirname(file_path))
df <- read.csv(file_path, sep=';', encoding='utf-8')

variables_list <- c('event_evaluation_centered', 'event_sentiment', 'event_sentiment_encoded', 'self_esteem', 'valence', 'calmness', 'id', 'group',
                    'event_category', 'ediary_type', 'event_subcategory')

output_base_path <- "<path to output directory>"

# setting reference levels
df$event_category <- factor(df$event_category, levels = c("Daily Routines and Household Activities", "Leisure and Recreation", "Social and Personal Relationships", "Work and Professional Engagements", "Health and Well-being", "Indeterminate"))
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

df$event_sentiment <- factor(df$event_sentiment, levels = c("negative", "neutral", "positive", "none"))
df$event_sentiment <- relevel(df$event_sentiment, ref = "neutral")

df$ediary_type <- factor(df$ediary_type, levels = c("1", "2"))
df$ediary_type <- relevel(df$ediary_type, ref = "2")

df$group <- factor(df$group, levels = c("1", "2"))
df$group <- relevel(df$group, ref = "2")

# selects relevant variables
analysis_df <- df[variables_list]

# resets the row index of the dataframe
analysis_df <- analysis_df[order(row.names(analysis_df)), ]

# checks
unique_ids <- unique(analysis_df$id)
print(unique_ids)

print(class(analysis_df$id))

data_types <- sapply(analysis_df, class)
print(data_types)

summary(analysis_df)


# ---------- reliability for Wilhelm & Schöbi (2007) two-item scales ----------
# scoring: valence = satisfied + (6 - unwell); calmness (tense-arousal axis) = relaxed + (6 - restless)
# orientation: higher = more positive valence / higher = calmer
# needs: dplyr, readr, tibble
two_item_alpha_sb <- function(r) ifelse(is.na(r), NA_real_, (2*r)/(1+r))

compute_reliabilities_2item <- function(dat, id, item_pos, item_rev, kmax = 6) {
  d <- dat %>%
    dplyr::select({{id}}, {{item_pos}}, {{item_rev}}) %>%
    dplyr::rename(i_pos = {{item_pos}}, i_neg = {{item_rev}}) %>%
    dplyr::mutate(i_neg = kmax - i_neg) # reverse-codes the negative pole
  
  # overall
  r_overall <- suppressWarnings(cor(d$i_pos, d$i_neg, use = "pairwise.complete.obs"))
  a_overall <- two_item_alpha_sb(r_overall)
  
  # between-person
  m <- d %>%
    dplyr::group_by({{id}}) %>%
    dplyr::summarise(i_pos = mean(i_pos, na.rm = TRUE),
                     i_neg = mean(i_neg, na.rm = TRUE),
                     .groups = "drop")
  r_between <- suppressWarnings(cor(m$i_pos, m$i_neg, use = "pairwise.complete.obs"))
  a_between <- two_item_alpha_sb(r_between)
  
  # within-person
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


# ----- identify items (0–6 scale in your data) -----
# valence: satisfied (positive pole) + unwell (reverse)
rel_valence <- compute_reliabilities_2item(
  df, id = id, item_pos = satisfied, item_rev = unwell, kmax = 6
) %>% dplyr::mutate(scale = "Valence (well & satisfied)")

# calmness (tense-arousal axis keyed as 'higher = calmer')
# calmness: relaxed (positive pole) + restless (reverse)
rel_calmness <- compute_reliabilities_2item(
  df, id = id, item_pos = relaxed, item_rev = restless, kmax = 6
) %>% dplyr::mutate(scale = "Calmness / tense-arousal (relaxed & low restless)")

# combine and save
reliab_out <- dplyr::bind_rows(rel_valence, rel_calmness) %>%
  dplyr::select(scale,
                inter_item_r_overall, alpha_overall,
                inter_item_r_between, alpha_between,
                inter_item_r_within,  alpha_within)

readr::write_csv(reliab_out, file.path(output_base_path, "reliability_summary.csv"))
print(reliab_out)


# ---------- self-esteem reliability (4 items; 0–9), McDonald's ω via multilevel CFA ----------
install_and_load_package("multilevelTools")
# reverse-score to higher = higher self-esteem
df <- df %>%
  dplyr::mutate(
    se_useless_r = 9 - .data$self_esteem_02_useless,
    se_failure_r = 9 - .data$self_esteem_03_failure
  )

# multilevel ω (within & between) using omegaSEM
omega_se <- multilevelTools::omegaSEM(
  items = c("self_esteem_01_satisfied", "self_esteem_04_positive_attitude",
            "se_useless_r", "se_failure_r"),
  id = "id",
  data = df,
  savemodel = FALSE
)
print(omega_se)

# (optional) keep your composite (0–9; higher = higher self-esteem)
df$self_esteem_pos <- rowMeans(
  cbind(df$self_esteem_01_satisfied,
        df$se_useless_r,
        df$se_failure_r,
        df$self_esteem_04_positive_attitude),
  na.rm = TRUE
)


# ---------- save summaries and APA table ----------
create_and_summarize_save_model <- function(dependent_var, formula_part, data, output_base_path, rq_number) {
  data$group <- factor(data$group, levels = c("1", "2"), labels = c("BPD", "HC"))
  data$group <- relevel(data$group, ref = "HC")
  
  formula <- as.formula(paste(dependent_var, formula_part))
  model <- lmerTest::lmer(formula, data = data, REML = FALSE)
  
  # note: REML TRUE was also carried, however, not reported in the manuscript due to exact same results.
  
  model_summary <- summary(model)

  apa_table <- model_summary$coefficients %>%
    as.data.frame() %>%
    rename(
      "b" = "Estimate",
      "SE" = "Std. Error",
      "t" = "t value",
      "p" = "Pr(>|t|)"
    ) %>%
    mutate(
      Effect = papaja::beautify_terms(rownames(.))
    ) %>%
    papaja::printnum(
      digits = c(4, 4, 4, 4, 4, 0),
      gt1 = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
      zero = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
    ) %>%
    select(Effect, `b`, `SE`, `t`, `df`, `p`)
  
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


# ------------- models -------------

# model 1 with 2-way interaction of EVENT and GROUP
# question: How context influence mood and self-esteem compared between groups (i.e., Reactivity)?

print("M1")

# goal: How the interaction between event categories and participant groups (those with Borderline Personality Disorder versus Healthy Controls)  associated to self-esteem, emotional valence, and calmness, while accounting for individual differences.
# interaction: Whether the effect of event categories on psychological outcomes varies between individuals with BPD and HCs.
# valence interaction removed: not significant

model_1_self_esteem <- create_and_summarize_save_model("self_esteem", "~ event_category + group + event_category:group + (1 | id)", analysis_df, output_base_path, 1)
model_1_valence <- create_and_summarize_save_model("valence", "~ event_category + group + (1 | id)", analysis_df, output_base_path, 1)
model_1_calmness <- create_and_summarize_save_model("calmness", "~ event_category + group + event_category:group + (1 | id)", analysis_df, output_base_path, 1)

print("M1-END")

# model 2 with 2-way interactions of SENTIMENT and GROUP
# question: What is the relation between events, sentiments and the outcome measures of valence, calmness and self-esteem?

# goal: How the interaction between event sentiment and participant groups (those with Borderline Personality Disorder versus Healthy Controls) associated to self-esteem, emotional valence, and calmness, while accounting for individual differences.
# interaction: Whether the effect of event sentiment (negative or positive) on psychological outcomes varies between individuals with BPD and HCs.
# calmness interaction removed: not significant

model_2_self_esteem <- create_and_summarize_save_model("self_esteem", "~ event_category + event_sentiment + group + event_sentiment:group + (1 | id)", analysis_df, output_base_path, 2)
model_2_valence <- create_and_summarize_save_model("valence", "~ event_category + event_sentiment + group + event_sentiment:group + (1 | id)", analysis_df, output_base_path, 2)
model_2_calmness <- create_and_summarize_save_model("calmness", "~ event_category + event_sentiment + group + (1 | id)", analysis_df, output_base_path, 2)

print("M2")

# model 3 Event Subcategory - EXPLORATORY

print("M3")

model_3_self_esteem <- create_and_summarize_save_model("self_esteem", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3)
model_3_valence <- create_and_summarize_save_model("valence", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3)
model_3_calmness <- create_and_summarize_save_model("calmness", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3)

print("M3-END")


# -------- bootstrap models ---------

print("Bootstrap Models Function")
set.seed(123)

create_and_summarize_bootstrap <- function(dependent_var, formula_part, data, output_base_path, rq_number, R = 10000) {

  data$group <- factor(data$group, levels = c("1", "2"), labels = c("BPD", "HC"))
  data$group <- relevel(data$group, ref = "HC")
  
  formula <- as.formula(paste(dependent_var, formula_part))
  
  # fits initial model to determine the number of fixed effects
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
  
  # initialising matrix to hold bootstrap coefficients
  num_fixed_effects <- length(fixef(initial_model))
  boot_coefs <- matrix(NA, nrow = R, ncol = num_fixed_effects)
  colnames(boot_coefs) <- names(fixef(initial_model))
  
  unique_ids <- unique(data$id)
  
  # clustered bootstrapping
  n_success <- 0L
  for (i in seq_len(R)) {
    sampled_ids <- sample(unique_ids, replace = TRUE)
    boot_data <- do.call(rbind, lapply(sampled_ids, function(s) data[data$id == s, , drop = FALSE]))
    boot_model <- tryCatch(
      lmerTest::lmer(formula, data = boot_data, REML = FALSE),
      error = function(e) NULL
    )
    if (!is.null(boot_model) && length(fixef(boot_model)) == num_fixed_effects) {
      n_success <- n_success + 1L
      boot_coefs[n_success, ] <- fixef(boot_model)
    }
  }

  # trimming
  if (n_success < R) {
    boot_coefs <- boot_coefs[seq_len(n_success), , drop = FALSE]
  }
  
  message(sprintf("Bootstrap fits succeeded: %d / %d (%.1f%%)", n_success, R, 100 * n_success / R))
  
  # calculates confidence intervals
  boot_cis <- apply(boot_coefs, 2, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))
  
  # compile bootstrap results into data frame
  bootstrap_results <- data.frame(Mean = apply(boot_coefs, 2, mean, na.rm = TRUE),
                                  CI_Lower = boot_cis["2.5%", ],
                                  CI_Upper = boot_cis["97.5%", ])
  
  bootstrap_results_file <- data.frame(Coefficient = colnames(boot_coefs), bootstrap_results)
  write.csv(bootstrap_results_file, paste0(output_base_path, "bootstrap_results_RQ", rq_number, "_", dependent_var, ".csv"), row.names = FALSE)
  
  return(bootstrap_results_file)
}
print("Bootstrap Models Function END")

# model 1 with 2-way Interaction
# question: How context influences mood compared between groups (i.e., Reactivity)?

print("BM1")

bootstrap_model_1_self_esteem <- create_and_summarize_bootstrap("self_esteem", "~ event_category + group + event_category:group + (1 | id)", analysis_df, output_base_path, 1, R = 10000)
bootstrap_model_1_valence <- create_and_summarize_bootstrap("valence", "~ event_category + group + (1 | id)", analysis_df, output_base_path, 1, R = 10000)
bootstrap_model_1_calmness <- create_and_summarize_bootstrap("calmness", "~ event_category + group + event_category:group + (1 | id)", analysis_df, output_base_path, 1, R = 10000)

print("BM1-END")

# model 2 with 2-way Interaction
# question: How is sentiment assotiated with self-esteem and mood measures of valence and calmness compared between groups (i.e., Reactivity)?

print("BM2")
print("BM2-1")
bootstrap_model_2_self_esteem <- create_and_summarize_bootstrap("self_esteem", "~ event_category + event_sentiment + group + event_sentiment:group + (1 | id)", analysis_df, output_base_path, 2, R = 10000)
print("BM2-2")
bootstrap_model_2_valence <- create_and_summarize_bootstrap("valence", "~ event_category + event_sentiment + event_sentiment:group + (1 | id)", analysis_df, output_base_path, 2, R = 10000)
print("BM2-3")
bootstrap_model_2_calmness <- create_and_summarize_bootstrap("calmness", "~ event_category + event_sentiment + group + (1 | id)", analysis_df, output_base_path, 2, R = 10000)

print("BM2-END")

# model 3
# Event Subcategories - EXPLORATORY MODEL

print("BM3s")
bootstrap_model_3_self_esteem <- create_and_summarize_bootstrap("self_esteem", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3, R = 10000)
print("BM3s END")
print("BM3v")
bootstrap_model_3_valence <- create_and_summarize_bootstrap("valence", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3, R = 10000)
print("BM3v END")
print("BM3c")
bootstrap_model_3_calmness <- create_and_summarize_bootstrap("calmness", "~ event_subcategory + group + (1 | id)", analysis_df, output_base_path, 3, R = 10000)
print("BM3c END")


# -------- evaluation of the models: R squared Marginal and Conditional ---------

calculate_r2_df <- function(model, model_name) {
  r2_values <- r.squaredGLMM(model)
  data.frame(
    Model = model_name,
    R2_Marginal = r2_values[1, "R2m"],
    R2_Conditional = r2_values[1, "R2c"]
  )
}

models <- list(
  model_1_self_esteem = model_1_self_esteem,
  model_1_valence = model_1_valence,
  model_1_calmness = model_1_calmness,
  model_2_self_esteem = model_2_self_esteem,
  model_2_valence = model_2_valence,
  model_2_calmness = model_2_calmness,
  model_3_self_esteem = model_3_self_esteem,
  model_3_valence = model_3_valence,
  model_3_calmness = model_3_calmness
)

results_df <- do.call(rbind, lapply(names(models), function(name) {
  calculate_r2_df(models[[name]], name)
}))

tryCatch({
  write.csv2(results_df, file = paste0(output_base_path, "Combined_R2_Results.csv"), row.names = FALSE, fileEncoding = "UTF-8")
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

# stop logging
sink(type = "output")
sink(type = "message")
close(log_conn)


# ---------- interaction plots for model  1 and 2 -----------
library(ggplot2)
library(ggtext)
library(htmltools)

df$event_sentiment <- factor(df$event_sentiment, levels = c("none", "negative", "neutral", "positive"))
levels(df$event_sentiment)


# two-way interaction plots: model 1
print("Models 1: 2-Way Interaction Plot -START")
create_two_way_interaction_plot_model1 <- function(model, dependentVar, output_base_path, 
                                                   plot_width, plot_height, 
                                                   x_text_size, y_text_size, y_limits,
                                                   figure_label, figure_number, figure_title) {

  analysis_df$event_category <- factor(analysis_df$event_category, levels = c(
    "Daily Routines and Household Activities", 
    "Leisure and Recreation", 
    "Social and Personal Relationships", 
    "Work and Professional Engagements", 
    "Health and Well-being", 
    "Indeterminate"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int")

  p$scales$scales <- p$scales$scales[!sapply(p$scales$scales, function(s) inherits(s, "ScaleDiscrete"))]
  
  p <- p +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10,10,10,10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Category", y = dependentVarLabel, color = "Group") +
    scale_x_discrete(
      limits = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate"),
      labels = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate Event")
    ) +
    coord_cartesian(ylim = y_limits)
  
  title_str <- paste0(
    "<strong style='font-family:Helvetica; font-size:12pt;'>", 
    figure_label, " ", figure_number,
    "</strong><br><br>",
    "<em style='font-family:Helvetica; font-size:14pt;'>", 
    figure_title,
    "</em>"
  )
  
  p <- p + ggtitle(HTML(title_str)) +
    theme(plot.title = element_markdown(hjust = 0))
  
  ggsave(file.path(output_base_path, paste("model_1", dependentVar, "interaction.png")),
         plot = p, width = plot_width, height = plot_height, dpi = 300)
}

# example calls for model 1:
# create_two_way_interaction_plot_model1(model_1_self_esteem, "Self esteem", output_base_path, 
#                                        10, 8, 10, 10, c(0, 9), 
#                                        figure_label = "Figure", 
#                                        figure_number = "2", 
#                                        figure_title = "Model 1 Self-esteem Interaction between Group and Event Category")
# 
# create_two_way_interaction_plot_model1(model_1_calmness, "Calmness", output_base_path, 
#                                        10, 8, 10, 10, c(0, 6), 
#                                        figure_label = "Figure", 
#                                        figure_number = "1", 
#                                        figure_title = "Model 1 Calmness Interaction between Group and Event Category")
#
 
print("Models 1: 2-Way Interaction Plots-END")


# two-way interaction plots: model 2
print("Models 2: 2-Way Interaction Plot -START")
create_two_way_interaction_plot_model2 <- function(model, dependentVar, output_base_path, 
                                                   plot_width, plot_height, 
                                                   x_text_size, y_text_size, y_limits,
                                                   figure_label, figure_number, figure_title) {

  analysis_df$event_sentiment <- factor(analysis_df$event_sentiment, 
                                        levels = c("negative", "neutral", "positive", "none"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int")

  p$scales$scales <- p$scales$scales[!sapply(p$scales$scales, function(s) inherits(s, "ScaleDiscrete"))]
  
  p <- p +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10,10,10,10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Sentiment", y = dependentVarLabel, color = "Group") +
    scale_x_discrete(
      limits = c("negative", "neutral", "positive", "none"),
      labels = c("Negative", "Neutral", "Positive", "Indeterminate Sentiment")
    ) +
    coord_cartesian(ylim = y_limits)
  
  title_str <- paste0(
    "<strong style='font-family:Helvetica; font-size:12pt;'>", 
    figure_label, " ", figure_number,
    "</strong><br><br>",
    "<em style='font-family:Helvetica; font-size:14pt;'>", 
    figure_title,
    "</em>"
  )
  
  p <- p + ggtitle(HTML(title_str)) +
    theme(plot.title = element_markdown(hjust = 0))
  
  ggsave(file.path(output_base_path, paste("model_2", dependentVar, "interaction.png")),
         plot = p, width = plot_width, height = plot_height, dpi = 300)
}

# example calls for model 2:
# create_two_way_interaction_plot_model2(model_2_self_esteem, "Self esteem", output_base_path, 
#                                        10, 8, 10, 10, c(0, 9), 
#                                        figure_label = "Figure", 
#                                        figure_number = "4", 
#                                        figure_title = "Model 2 Self-esteem Interaction between Group and Sentiment")
# 
# create_two_way_interaction_plot_model2(model_2_valence, "Valence", output_base_path, 
#                                        10, 8, 10, 10, c(0, 6), 
#                                        figure_label = "Figure", 
#                                        figure_number = "3", 
#                                        figure_title = "Model 2 Valence Interaction between Group and Sentiment")
# 
# print("Models 2: 2-Way Interaction Plots-END")


# ---------- combined -----------
library(ggplot2)
library(ggtext)
library(htmltools)
library(patchwork)

create_two_way_interaction_plot_model1_plot <- function(model, dependentVar, 
                                                        x_text_size, y_text_size, y_limits,
                                                        subfigure_label, figure_title) {
  analysis_df$event_category <- factor(analysis_df$event_category, levels = c(
    "Daily Routines and Household Activities", 
    "Leisure and Recreation", 
    "Social and Personal Relationships", 
    "Work and Professional Engagements", 
    "Health and Well-being", 
    "Indeterminate"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int")

  p$scales$scales <- p$scales$scales[!sapply(p$scales$scales, function(s) inherits(s, "ScaleDiscrete"))]
  
  p <- p +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10, 10, 10, 10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Category", y = dependentVarLabel, color = "Group") +
    scale_x_discrete(
      limits = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate"),
      labels = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate Event")
    ) +
    coord_cartesian(ylim = y_limits)

  return(p)
}

create_two_way_interaction_plot_model2_plot <- function(model, dependentVar, 
                                                        x_text_size, y_text_size, y_limits,
                                                        subfigure_label, figure_title) {

  analysis_df$event_sentiment <- factor(analysis_df$event_sentiment, 
                                        levels = c("negative", "neutral", "positive", "none"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int")

  p$scales$scales <- p$scales$scales[!sapply(p$scales$scales, function(s) inherits(s, "ScaleDiscrete"))]
  
  p <- p +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10, 10, 10, 10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Sentiment", y = dependentVarLabel, color = "Group") +
    scale_x_discrete(
      limits = c("negative", "neutral", "positive", "none"),
      labels = c("Negative", "Neutral", "Positive", "Indeterminate Sentiment")
    ) +
    coord_cartesian(ylim = y_limits)

  return(p)
}

# example call: model 1, combined figure of calmness and self-esteem
# plot_model1_a <- create_two_way_interaction_plot_model1_plot(model_1_self_esteem, "Self esteem", 
#                                                              x_text_size = 10, y_text_size = 10, y_limits = c(0, 9),
#                                                              subfigure_label = "Figure 1A", 
#                                                              figure_title = "Model 1 Self-esteem Interaction between Group and Event Category")
# plot_model1_b <- create_two_way_interaction_plot_model1_plot(model_1_calmness, "Calmness", 
#                                                              x_text_size = 10, y_text_size = 10, y_limits = c(0, 6),
#                                                              subfigure_label = "Figure 1B", 
#                                                              figure_title = "Model 1 Calmness Interaction between Group and Event Category")
# 
# combined_model1 <- plot_model1_b + plot_model1_a + 
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")
# 
# ggsave(file.path(output_base_path, "model_1_combined_interaction.png"),
#        plot = combined_model1, width = 12, height = 8, dpi = 300)

# example call: model 2 combined figure of valence and self-esteem
# plot_model2_a <- create_two_way_interaction_plot_model2_plot(model_2_self_esteem, "Self esteem", 
#                                                              x_text_size = 10, y_text_size = 10, y_limits = c(0, 9),
#                                                              subfigure_label = "Figure 2A", 
#                                                              figure_title = "Model 2 Self-esteem Interaction between Group and Sentiment")
# plot_model2_b <- create_two_way_interaction_plot_model2_plot(model_2_valence, "Valence", 
#                                                              x_text_size = 10, y_text_size = 10, y_limits = c(0, 6),
#                                                              subfigure_label = "Figure 2B", 
#                                                              figure_title = "Model 2 Valence Interaction between Group and Sentiment")
# 
# combined_model2 <- plot_model2_b + plot_model2_a + 
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")
# 
# ggsave(file.path(output_base_path, "model_2_combined_interaction.png"),
#        plot = combined_model2, width = 10, height = 8, dpi = 300)


# ---------- interaction plots -----------
library(ggplot2)
library(ggtext)  # For element_markdown

library(ggplot2)
library(ggtext)   # For element_markdown
library(htmltools)  # For HTML()

df$event_sentiment <- factor(df$event_sentiment, levels = c("none", "negative", "neutral", "positive"))
levels(df$event_sentiment)


# two-way interaction plot: model 1
print("Models 1: 2-Way Interaction Plot -START")
create_two_way_interaction_plot_model1 <- function(model, dependentVar, output_base_path, 
                                                   plot_width, plot_height, 
                                                   x_text_size, y_text_size, y_limits,
                                                   figure_label, figure_number, figure_title) {

  analysis_df$event_category <- factor(analysis_df$event_category, levels = c(
    "Daily Routines and Household Activities", 
    "Leisure and Recreation", 
    "Social and Personal Relationships", 
    "Work and Professional Engagements", 
    "Health and Well-being", 
    "Indeterminate"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10,10,10,10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Category", y = dependentVarLabel, color = "Group") +
    suppressWarnings(scale_x_discrete(
      limits = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate"),
      labels = c("Daily Routines and Household Activities", 
                 "Leisure and Recreation", 
                 "Social and Personal Relationships", 
                 "Work and Professional Engagements", 
                 "Health and Well-being", 
                 "Indeterminate Event")
    )) +
    coord_cartesian(ylim = y_limits)
  
  title_str <- paste0(
    "<strong style='font-family:Helvetica; font-size:12pt;'>", 
    figure_label, " ", figure_number,
    "</strong><br><br>",
    "<em style='font-family:Helvetica; font-size:14pt;'>", 
    figure_title,
    "</em>"
  )
  
  p <- p + ggtitle(HTML(title_str)) +
    theme(plot.title = element_markdown(hjust = 0))
  
  ggsave(file.path(output_base_path, paste("model_1", dependentVar, "interaction.png")),
         plot = p, width = plot_width, height = plot_height, dpi = 300)
}

# example calls for model 1:
# create_two_way_interaction_plot_model1(model_1_self_esteem, "Self esteem", output_base_path, 
#                                        10, 8, 10, 10, c(0, 9), 
#                                        figure_label = "Figure", 
#                                        figure_number = "2", 
#                                        figure_title = "Model 1 Self-esteem Interaction between Group and Event Category")
# 
# create_two_way_interaction_plot_model1(model_1_calmness, "Calmness", output_base_path, 
#                                        10, 8, 10, 10, c(0, 6), 
#                                        figure_label = "Figure", 
#                                        figure_number = "1", 
#                                        figure_title = "Model 1 Calmness Interaction between Group and Event Category")
 
print("Models 1: 2-Way Interaction Plots-END")


# two-way interaction plot: model 2
print("Models 2: 2-Way Interaction Plot -START")
create_two_way_interaction_plot_model2 <- function(model, dependentVar, output_base_path, 
                                                   plot_width, plot_height, 
                                                   x_text_size, y_text_size, y_limits,
                                                   figure_label, figure_number, figure_title) {

  analysis_df$event_sentiment <- factor(analysis_df$event_sentiment, 
                                        levels = c("negative", "none", "neutral", "positive"))
  
  dependentVarLabel <- if (dependentVar == "Self esteem") "Self-esteem" else dependentVar
  
  p <- plot_model(model, type = "int") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = x_text_size),
          axis.text.y = element_text(size = y_text_size),
          plot.margin = margin(10,10,10,10, "mm"),
          text = element_text(family = "Helvetica", size = 12)) +
    labs(x = "Event Sentiment", y = dependentVarLabel, color = "Group") +
    suppressWarnings(scale_x_discrete(
      limits = c("negative", "none", "neutral", "positive"),
      labels = c("Negative", "Indeterminate Sentiment", "Neutral", "Positive")
    )) +
    coord_cartesian(ylim = y_limits)
  
  title_str <- paste0(
    "<strong style='font-family:Helvetica; font-size:12pt;'>", 
    figure_label, " ", figure_number,
    "</strong><br><br>",
    "<em style='font-family:Helvetica; font-size:14pt;'>", 
    figure_title,
    "</em>"
  )
  
  p <- p + ggtitle(HTML(title_str)) +
    theme(plot.title = element_markdown(hjust = 0))
  
  ggsave(file.path(output_base_path, paste("model_2", dependentVar, "interaction.png")),
         plot = p, width = plot_width, height = plot_height, dpi = 300)
}

# example calls for model 2:
# create_two_way_interaction_plot_model2(model_2_self_esteem, "Self esteem", output_base_path, 
#                                        10, 8, 10, 10, c(0, 9), 
#                                        figure_label = "Figure", 
#                                        figure_number = "4", 
#                                        figure_title = "Model 2 Self-esteem Interaction between Group and Sentiment")
# 
# create_two_way_interaction_plot_model2(model_2_valence, "Valence", output_base_path, 
#                                        10, 8, 10, 10, c(0, 6), 
#                                        figure_label = "Figure", 
#                                        figure_number = "3", 
#                                        figure_title = "Model 2 Valence Interaction between Group and Sentiment")

print("Models 2: 2-Way Interaction Plots-END")