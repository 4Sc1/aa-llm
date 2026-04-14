
# Inter-rater Reliability Analyses 
# Fleiss' kappa, Krippendorff's alpha,bootstrap CIs, and pairwise Cohen's kappa
#
# Notes
# - Expects CSVs with semicolon separators and UTF-8 encoding.
# - Saves outputs
# - Bootstrap CI uses percentile intervals.
# - Seed for bootstrap is fixed at 123.
# 
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#
# Author: David Levi Tekampe
# University of Luxembourg

install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    message(sprintf("Installing package: %s", package_name))
    tryCatch({
      install.packages(package_name, dependencies = TRUE)
      library(package_name, character.only = TRUE)
      message(sprintf("Package %s installed and loaded.", package_name))
    }, error = function(e) {
      message(sprintf("Error installing/loading %s: %s", package_name, e$message))
    })
  } else {
    library(package_name, character.only = TRUE)
    message(sprintf("Package %s loaded.", package_name))
  }
}

required_packages <- c("irr", "psych", "raters", "irrCAC", "boot")
invisible(lapply(required_packages, install_and_load_package))


krippendorff_alpha_file_path <- "<path to results folder>/rating_krippendorffsalpha.csv"
fleiss_kappa_file_path      <- "<path to results folder>/reshaped_for_fleiss_kappa.csv"

fleiss_kappa_txt_path       <- "<path to results folder>/fleiss_kappa_results.txt"
fleiss_kappa_ci_txt_path    <- "<path to results folder>/fleiss_kappa_ci_results.txt"
bootstrap_ci_txt_path       <- "<path to results folder>/bootstrap_fleiss_kappa_ci_results.txt"
kripp_alpha_txt_path        <- "krippendorff_alpha_results.txt"


fleiss_data <- read.csv(fleiss_kappa_file_path, sep = ";", encoding = "UTF-8")

message("Fleiss' kappa input preview and structure:")
print(utils::head(fleiss_data))
str(fleiss_data)

krippendorff_data <- read.csv(krippendorff_alpha_file_path, sep = ";", encoding = "UTF-8")


fleiss_ratings_only <- fleiss_data[, -1]

fleiss_kappa_res <- irr::kappam.fleiss(fleiss_ratings_only, detail = TRUE)

print(fleiss_kappa_res)
writeLines(capture.output(print(fleiss_kappa_res)), fleiss_kappa_txt_path)


fleiss_kappa_result <- irrCAC::fleiss.kappa.raw(
  fleiss_ratings_only,
  weights = "unweighted",
  conflev = 0.95
)
print(fleiss_kappa_result)

fleiss_kappa_value <- fleiss_kappa_result$est$coeff.val
fleiss_kappa_ci    <- fleiss_kappa_result$est$conf.int
cat("Fleiss' Kappa: ", fleiss_kappa_value, "\n")
cat("Confidence Interval: ", fleiss_kappa_ci, "\n")

fleiss_kappa_ci_output <- capture.output({
  print(fleiss_kappa_result)
  cat("Fleiss' Kappa: ", fleiss_kappa_value, "\n")
  cat("Confidence Interval: ", fleiss_kappa_ci, "\n")
})
writeLines(fleiss_kappa_ci_output, fleiss_kappa_ci_txt_path)


krippendorff_matrix <- t(as.matrix(krippendorff_data))
kripp_alpha_res     <- irr::kripp.alpha(krippendorff_matrix)

print(kripp_alpha_res)
writeLines(capture.output(print(kripp_alpha_res)), kripp_alpha_txt_path)


bootstrap_fleiss_kappa <- function(data, indices) {
  resampled <- data[indices, , drop = FALSE]
  irr::kappam.fleiss(resampled)$value
}

fleiss_data_matrix <- as.matrix(fleiss_ratings_only)

set.seed(123)  
boot_res <- boot::boot(data = fleiss_data_matrix, statistic = bootstrap_fleiss_kappa, R = 1000)
print(boot_res)

boot_ci <- boot::boot.ci(boot_res, type = "perc")
print(boot_ci)

bootstrap_ci_output <- capture.output({
  print(boot_res)
  print(boot_ci)
})
writeLines(bootstrap_ci_output, bootstrap_ci_txt_path)


if (all(c("Rater_LLM", "Rater_A", "Rater_D", "Rater_P") %in% names(fleiss_data))) {
  fleiss_data$Rater_LLM <- as.factor(fleiss_data$Rater_LLM)
  fleiss_data$Rater_A   <- as.factor(fleiss_data$Rater_A)
  fleiss_data$Rater_D   <- as.factor(fleiss_data$Rater_D)
  fleiss_data$Rater_P   <- as.factor(fleiss_data$Rater_P)
}

calculate_pairwise_kappa <- function(data) {
  num_raters <- ncol(data)
  kappa_mat  <- matrix(NA_real_, nrow = num_raters, ncol = num_raters,
                       dimnames = list(colnames(data), colnames(data)))
  for (i in 1:(num_raters - 1)) {
    for (j in (i + 1):num_raters) {
      kappa_mat[i, j] <- irr::kappa2(data[, c(i, j)])$value
    }
  }
  kappa_mat
}

pairwise_kappa_results <- calculate_pairwise_kappa(fleiss_data[, -1])  
print(pairwise_kappa_results)

