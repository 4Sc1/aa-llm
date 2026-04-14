
# Bayesian Analyses (brms)
#
# Notes
# - Expects CSVs with semicolon separators and UTF-8 encoding.
# - Mirrors LME preprocessing + model specifications (M1–M3).
#
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#
# Author: David Levi Tekampe
# University of Luxembourg

set.seed(123)

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

required_packages <- c(
  "dplyr", "brms", "bayesplot", "tidybayes",
  "ggplot2", "naniar", "report", "distributional"
)

invisible(lapply(required_packages, install_and_load_package))


file_path <- "<dataset input path>"
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

df$event_sentiment <- factor(df$event_sentiment, levels = c("negative", "neutral", "positive", "none"))
df$event_sentiment <- relevel(df$event_sentiment, ref = "neutral")

df$ediary_type <- factor(df$ediary_type, levels = c("1", "2"))
df$ediary_type <- relevel(df$ediary_type, ref = "2")

df$group <- factor(df$group, levels = c("1", "2"), labels = c("BPD", "HC"))
df$group <- relevel(df$group, ref = "HC")

if (!"event_sentiment_encoded" %in% names(df)) {
  stop("Column 'event_sentiment_encoded' not found in df. It should code sentiment as -1, 0, 1, NA.")
}
df$event_sentiment_encoded <- as.numeric(df$event_sentiment_encoded)

df <- df %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    sentiment_mean = mean(event_sentiment_encoded, na.rm = TRUE),
    sentiment_mean = dplyr::if_else(is.nan(sentiment_mean), NA_real_, sentiment_mean),
    sentiment_within = event_sentiment_encoded - sentiment_mean
  ) %>%
  dplyr::ungroup()

variables_list <- c(
  "event_category",
  "event_subcategory",
  "event_sentiment",
  "event_sentiment_encoded",
  "sentiment_mean",
  "sentiment_within",
  "self_esteem",
  "valence",
  "calmness",
  "id",
  "group",
  "ediary_type"
)

analysis_df <- df[variables_list]
analysis_df$id <- factor(analysis_df$id)

output_base_path <- "<bayes output path>"
dir.create(output_base_path, showWarnings = FALSE, recursive = TRUE)

summary(analysis_df)


analysis_df$group <- factor(analysis_df$group, levels = c("1", "2"))
analysis_df$id <- factor(analysis_df$id)

priors <- c(
  set_prior("normal(0, 5)", class = "b"),
  set_prior("student_t(3, 0, 10)", class = "Intercept"),
  set_prior("student_t(3, 0, 5)", class = "sd", lb = 0)
)

fit_brm <- function(formula, data, file_stub) {
  data <- data %>%
    dplyr::mutate(
      group = factor(group, levels = c("1", "2"), labels = c("BPD", "HC")),
      group = stats::relevel(group, ref = "HC"),
      id = factor(id)
    )
  
  brm(
    formula = formula,
    data = data,
    family = gaussian(),
    prior = priors,
    iter = 20000,
    warmup = 2000,
    chains = 4,
    cores = 4,
    control = list(adapt_delta = 0.80),
    file = file.path(output_base_path, file_stub)
  )
}


model_1_self_esteem <- fit_brm(
  self_esteem ~ event_category + group + event_category:group + (1 | id),
  analysis_df,
  "model_1_self_esteem"
)

model_1_valence <- fit_brm(
  valence ~ event_category + group + (1 | id),
  analysis_df,
  "model_1_valence"
)

model_1_calmness <- fit_brm(
  calmness ~ event_category + group + event_category:group + (1 | id),
  analysis_df,
  "model_1_calmness"
)


model_2_self_esteem <- fit_brm(
  self_esteem ~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id),
  analysis_df,
  "model_2_self_esteem"
)

model_2_valence <- fit_brm(
  valence ~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id),
  analysis_df,
  "model_2_valence"
)

model_2_calmness <- fit_brm(
  calmness ~ event_category + group + sentiment_mean + sentiment_within * group + (1 | id),
  analysis_df,
  "model_2_calmness"
)


model_3_self_esteem <- fit_brm(
  self_esteem ~ event_subcategory + group + (1 | id),
  analysis_df,
  "model_3_self_esteem"
)

model_3_valence <- fit_brm(
  valence ~ event_subcategory + group + (1 | id),
  analysis_df,
  "model_3_valence"
)

model_3_calmness <- fit_brm(
  calmness ~ event_subcategory + group + (1 | id),
  analysis_df,
  "model_3_calmness"
)


save_brms_summary_as_csv <- function(model, model_name, output_dir) {
  s <- summary(model)
  
  fixed_effects_df <- as.data.frame(s$fixed)
  fixed_effects_df$Parameter <- rownames(fixed_effects_df)
  rownames(fixed_effects_df) <- NULL
  
  fixed_effects_df$Group <- ifelse(grepl("^groupBPD$|groupBPD", fixed_effects_df$Parameter), "BPD", "HC")
  
  fixed_effects_df <- fixed_effects_df[, c(
    "Parameter", "Group", "Estimate", "Est.Error",
    "l-95% CI", "u-95% CI", "Rhat", "Bulk_ESS", "Tail_ESS"
  )]
  
  csv_path <- file.path(output_dir, paste0(model_name, "_fixed_effects.csv"))
  write.csv(fixed_effects_df, csv_path, row.names = FALSE)
  message(paste("Saved fixed effects summary to:", csv_path))
}

models <- list(
  model_1_self_esteem = model_1_self_esteem,
  model_1_valence     = model_1_valence,
  model_1_calmness    = model_1_calmness,
  model_2_self_esteem = model_2_self_esteem,
  model_2_valence     = model_2_valence,
  model_2_calmness    = model_2_calmness,
  model_3_self_esteem = model_3_self_esteem,
  model_3_valence     = model_3_valence,
  model_3_calmness    = model_3_calmness
)

for (nm in names(models)) {
  save_brms_summary_as_csv(models[[nm]], nm, output_base_path)
}
