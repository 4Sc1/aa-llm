
# Bayesian Analyses 
#
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#
# Notes
#    - Expects CSVs with semicolon separators and UTF-8 encoding.
#    - Saves outputs
#
# Author: David Levi Tekampe
# University of Luxembourg


# --------------------- config ---------------------
set.seed(123)

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

required_packages <- c("dplyr", "lme4", "pROC", "ggplot2", "car", "knitr", "ggeffects", "viridis", "lmerTest", "ggdist", "ggpubr", "lattice", "lubridate", "patchwork", "papaja", "grid", "gridExtra", "magrittr", "forecast", "sjPlot", "webshot", "MuMIn", "brms", "magrittr", "bayesplot", "naniar", "report", "tidybayes", "distributional", "plotly")

invisible(lapply(required_packages, install_and_load_package))

# ----------- loading and pre-processing -----------
file_path <- "<path to input CSV file>"
setwd(dirname(file_path))
df <- read.csv(file_path, sep=';', encoding='utf-8', quote="")


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
df$id <- factor(df$id)

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


# ---------- Bayesian Models using brms ----------

# priors for Likert-like scale data
priors <- c(
  set_prior("normal(0, 5)", class = "b"),
  set_prior("student_t(3, 0, 10)", class = "Intercept"),
  set_prior("student_t(3, 0, 5)", class = "sd", lb = 0)
)

# note: 
# Model 3 was not included in the manuscript. 
# Model 4 is mentioned as Model 3 in the manuscript which is for the exploratory analysis of event subcategories
# To preserve the scripts integrity, the model names in the script were not changed. 
# Thus, Model 3 in the manuscript is Model 4 in the script

# model 1: interaction between event_category and group on self-esteem, valence, and calmness
model_1_self_esteem <- brm(
  self_esteem ~ event_category + group + event_category:group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_1_self_esteem")
)

model_1_valence <- brm(
  valence ~ event_category + group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_1_valence")
)

model_1_calmness <- brm(
  calmness ~ event_category + group + event_category:group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_1_calmness")
)


# model 2: interaction between event_sentiment and group on self-esteem, valence, and calmness
model_2_self_esteem <- brm(
  self_esteem ~ event_category + event_sentiment + group + event_sentiment:group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_2_self_esteem")
)

model_2_valence <- brm(
  valence ~ event_category + event_sentiment + group + event_sentiment:group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_2_valence")
)

model_2_calmness <- brm(
  calmness ~ event_category + event_sentiment + group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_2_calmness")
)


model_3_self_esteem <- brm(
  self_esteem ~ event_subcategory + group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_3_self_esteem")
)

model_3_valence <- brm(
  valence ~ event_subcategory + group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_3_valence")
)

model_3_calmness <- brm(
  calmness ~ event_subcategory + group + (1 | id),
  data = df,
  family = gaussian(),
  prior = priors,
  iter = 20000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.80),
  file = paste0(output_base_path, "model_3_calmness")
)


# ---------- helpers ----------
save_brms_summary_as_csv <- function(model, model_name, output_dir) {
  model_summary <- summary(model)
  
  fixed_effects_df <- as.data.frame(model_summary$fixed)
  fixed_effects_df$Parameter <- rownames(fixed_effects_df)
  rownames(fixed_effects_df) <- NULL
  
  # Group column: if the parameter name contains "group1", mark as "BPD"; otherwise, assume it pertains to the reference group "HC"
  fixed_effects_df$Group <- ifelse(grepl("group1", fixed_effects_df$Parameter), "BPD", "HC")
  
  fixed_effects_df <- fixed_effects_df[, c("Parameter", "Group", "Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Rhat", "Bulk_ESS", "Tail_ESS")]
  
  csv_path <- file.path(output_dir, paste0(model_name, "_fixed_effects.csv"))
  write.csv(fixed_effects_df, csv_path, row.names = FALSE)
  
  message(paste("Saved fixed effects summary to:", csv_path))
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
  model_3_calmness = model_3_calmness,
)

# ---------- generate results ----------
for(model_name in names(models)) {
  save_brms_summary_as_csv(models[[model_name]], model_name, output_base_path)
}