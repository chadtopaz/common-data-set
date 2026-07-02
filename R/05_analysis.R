# ===============================================================================
# LEGACY ADMISSIONS AND URM ENROLLMENT: Final Analysis for Paper
#
# This script produces all results for the paper examining the association between
# legacy admissions emphasis and underrepresented minority enrollment.
#
# Key design decisions (justified in Methods):
#   1. PRIMARY SPECIFICATION: Binary treatment (considers legacy vs. does not)
#      - Matches actual data structure: 8 schools "Not Considered", 27 "Considered", 1 "Important"
#      - Avoids extrapolation beyond observed support
#   2. Cross-sectional design averaging over years (justified by panel analysis
#      showing most variation is between schools, not within)
#   3. Controls: sector + 3 PCs of other admissions priorities (numeric PCA baseline)
#   4. Ordinal robustness: polychoric PCA on other admissions priorities (appendix/robustness)
#   5. HC2 robust standard errors with t-based inference
#
# Outputs:
#   MAIN TEXT:
#     - Table 1: Descriptive statistics
#     - Table 2: Main results (binary, both samples)
#     - Table 3: Robustness summary (includes polychoric PCA line + PC sensitivity for binary)
#     - Figure 1: URM by legacy category (binned means)
#   APPENDIX:
#     - Table A1. Sector × legacy policy (primary sample; counts with row percentages)
#     - Table A2. URM enrollment descriptives within the public sector, by legacy policy
#     - Table A3. Full regression coefficients: primary specification (HC2 SEs)
#     - Table A4. Full regression coefficients: ordinal robustness using polychoric PCA
#     - Table A5. Principal component loadings for non-legacy admissions priorities (top five absolute loadings per PC)
#     - Table A6. Panel diagnostics summary (variance decomposition and estimators)
#     - Figure A1. Scatter plot (legacy_z vs URM) with regression line
#     - Figure A2. Leave-one-out distribution (binary)
# ===============================================================================

library(tidyverse)
library(janitor)
library(scales)
library(sandwich)
library(plm)
library(psych)

set.seed(42)
options(dplyr.summarise.inform = FALSE)

# ===============================================================================
# CONFIG
# ===============================================================================

INPUT_FILE <- "data/Final_2010_2022.csv"
MIN_YEARS_PRIMARY <- 8
MIN_YEARS_BALANCED <- 13
N_PCS <- 3
SE_TYPE <- "HC2"

OUT_DIR <- "output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "tables"), showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "appendix"), showWarnings = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

# ===============================================================================
# HELPER: Robust coefficient table with HC2 and t-based inference
# ===============================================================================

coef_table_hc <- function(model, type = "HC2") {
  V <- sandwich::vcovHC(model, type = type)
  est <- coef(model)
  se <- sqrt(diag(V))
  t_stat <- est / se
  df <- df.residual(model)
  p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  crit <- qt(0.975, df = df)
  
  tibble(
    term = names(est),
    estimate = as.numeric(est),
    se = as.numeric(se),
    t = as.numeric(t_stat),
    df = df,
    p = as.numeric(p_val),
    ci_lower = estimate - crit * se,
    ci_upper = estimate + crit * se
  )
}

# ===============================================================================
# HELPER: Polychoric PCA scores (ordinal robustness)
#   - Uses school-level mean priorities (excluding legacy), treated as ordinal-coded
#   - Returns PC1_poly, PC2_poly, PC3_poly (regression-style scores)
#   - Robust to missing/degenerate variables in small samples (drops problematic vars)
# ===============================================================================

add_polychoric_pcs <- function(school_df, priority_vars, n_factors = 3) {
  
  X_full <- school_df %>%
    dplyr::select(all_of(priority_vars)) %>%
    mutate(across(everything(), as.numeric))
  
  keep_rows <- complete.cases(X_full)
  X <- X_full[keep_rows, , drop = FALSE]
  
  usable <- map_lgl(X, function(v) {
    v <- v[!is.na(v)]
    length(unique(v)) >= 2
  })
  X <- X[, usable, drop = FALSE]
  
  out <- school_df %>%
    mutate(
      PC1_poly = NA_real_,
      PC2_poly = NA_real_,
      PC3_poly = NA_real_
    )
  
  if (ncol(X) < 2 || nrow(X) < 5) return(out)
  
  for (attempt in 1:2) {
    
    poly <- suppressWarnings(psych::polychoric(as.data.frame(X), correct = 0, smooth = TRUE))
    rho <- poly$rho
    
    if (anyNA(rho)) {
      bad <- which(apply(rho, 2, function(col) anyNA(col)))
      if (length(bad) >= ncol(X)) return(out)
      X <- X[, -bad, drop = FALSE]
      if (ncol(X) < 2) return(out)
      next
    }
    
    k <- min(n_factors, ncol(X))
    Xs <- scale(X)
    
    pca_poly <- psych::principal(rho, nfactors = k, rotate = "none")
    
    scores_poly <- Xs %*% pca_poly$loadings
    scores_poly <- as_tibble(scores_poly)
    colnames(scores_poly) <- paste0("PC", 1:k, "_poly")
    
    tmp <- matrix(NA_real_, nrow = sum(keep_rows), ncol = 3)
    tmp[, 1:k] <- as.matrix(scores_poly)
    
    out[keep_rows, c("PC1_poly", "PC2_poly", "PC3_poly")] <- tmp
    return(out)
  }
  
  out
}

# ===============================================================================
# 1) LOAD AND PREPARE DATA
# ===============================================================================

cat("=== LOADING DATA ===\n")

data_raw_all <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  clean_names()

# Handle apostrophe variant if present
if ("level_of_applicant_s_interest" %in% names(data_raw_all) &&
    !"level_of_applicants_interest" %in% names(data_raw_all)) {
  data_raw_all <- data_raw_all %>%
    rename(level_of_applicants_interest = level_of_applicant_s_interest)
}

legacy_var <- "alumni_ae_relation"
priority_cols_all <- c(
  "rigor_of_secondary_school_record", "class_rank", "academic_gpa",
  "standardized_test_scores", "application_essay", "recommendation_s",
  "interview", "extracurricular_activities", "talent_ability",
  "character_personal_qualities", "first_generation", "alumni_ae_relation",
  "geographical_residence", "state_residency", "religious_affiliation_commitment",
  "racial_ethnic_status", "volunteer_work", "work_experience",
  "level_of_applicants_interest"
)

priority_cols <- intersect(priority_cols_all, names(data_raw_all))
cat(sprintf("Found %d of %d priority columns\n", length(priority_cols), length(priority_cols_all)))

other_priorities <- setdiff(priority_cols, legacy_var)

fy_cols <- names(data_raw_all)[str_detect(names(data_raw_all), "^degree_seeking_first_year_")]
urm_cols <- c(
  "degree_seeking_first_year_aian",
  "degree_seeking_first_year_black",
  "degree_seeking_first_year_latinx",
  "degree_seeking_first_year_nhpi"
)
urm_cols <- intersect(urm_cols, names(data_raw_all))

# ===============================================================================
# 2) CREATE ANALYSIS DATASETS
# ===============================================================================

create_school_data <- function(data_all, min_years) {
  
  data_filtered <- data_all %>%
    group_by(school_name) %>%
    filter(n() >= min_years) %>%
    ungroup()
  
  year_data <- data_filtered %>%
    mutate(
      legacy_rating = to_num(.data[[legacy_var]]),
      across(all_of(fy_cols), to_num),
      across(all_of(priority_cols), to_num),
      year_num = as.numeric(str_extract(year, "^\\d{4}"))
    ) %>%
    mutate(
      total_fy = rowSums(across(all_of(fy_cols)), na.rm = TRUE),
      urm_count = rowSums(across(all_of(urm_cols)), na.rm = TRUE),
      urm_prop = ifelse(total_fy > 0, urm_count / total_fy, NA_real_)
    ) %>%
    filter(!is.na(legacy_rating), !is.na(urm_prop), total_fy > 0)
  
  school_data <- year_data %>%
    group_by(school_name) %>%
    summarise(
      legacy_mean = mean(legacy_rating, na.rm = TRUE),
      legacy_modal = as.numeric(names(sort(table(legacy_rating), decreasing = TRUE))[1]),
      urm_prop = mean(urm_prop, na.rm = TRUE),
      across(all_of(other_priorities), ~ mean(.x, na.rm = TRUE)),
      public_private = first(public_private),
      n_years = n(),
      .groups = "drop"
    ) %>%
    filter(
      is.finite(legacy_mean),
      is.finite(urm_prop),
      !is.na(public_private)
    ) %>%
    mutate(
      considers_legacy = factor(
        legacy_modal >= 2,
        levels = c(FALSE, TRUE),
        labels = c("Does not consider", "Considers")
      )
    )
  
  school_scaled <- school_data %>%
    mutate(
      legacy_z = as.numeric(scale(legacy_mean)),
      across(all_of(other_priorities), ~ as.numeric(scale(.x)), .names = "{.col}_z")
    )
  
  X_other <- school_scaled %>%
    dplyr::select(ends_with("_z"), -legacy_z) %>%
    as.matrix()
  
  pca_result <- prcomp(X_other, center = FALSE, scale. = FALSE)
  
  n_pcs_available <- min(6, ncol(pca_result$x))
  pc_scores <- pca_result$x[, 1:n_pcs_available, drop = FALSE]
  colnames(pc_scores) <- paste0("PC", 1:n_pcs_available)
  
  school_final <- school_scaled %>%
    bind_cols(as_tibble(pc_scores))
  
  # Add polychoric PCs computed on un-z-scored means (ordinal-coded)
  school_final <- school_final %>%
    left_join(
      add_polychoric_pcs(
        school_df = school_data,
        priority_vars = other_priorities,
        n_factors = 3
      ) %>% select(school_name, PC1_poly, PC2_poly, PC3_poly),
      by = "school_name"
    )
  
  list(
    school_data = school_final,
    year_data = year_data,
    pca_result = pca_result,
    n_schools = nrow(school_final)
  )
}

cat("Creating primary sample (>= 8 years)...\n")
primary <- create_school_data(data_raw_all, MIN_YEARS_PRIMARY)
cat(sprintf("  n = %d schools\n", primary$n_schools))

cat("Creating balanced sample (13 years)...\n")
balanced <- create_school_data(data_raw_all, MIN_YEARS_BALANCED)
cat(sprintf("  n = %d schools\n", balanced$n_schools))

# ===============================================================================
# 3) TABLE 1: DESCRIPTIVE STATISTICS
# ===============================================================================

cat("\n=== TABLE 1: DESCRIPTIVE STATISTICS ===\n")

legacy_dist <- primary$school_data %>%
  count(considers_legacy) %>%
  mutate(pct = 100 * n / sum(n))

cat("\nLegacy emphasis distribution:\n")
print(legacy_dist)

urm_by_legacy <- primary$school_data %>%
  group_by(considers_legacy) %>%
  summarise(
    n = n(),
    mean_urm = mean(urm_prop),
    sd_urm = sd(urm_prop),
    min_urm = min(urm_prop),
    max_urm = max(urm_prop),
    .groups = "drop"
  )

cat("\nURM enrollment by legacy emphasis:\n")
print(urm_by_legacy)

overall_desc <- primary$school_data %>%
  summarise(
    n_schools = n(),
    n_public = sum(public_private == "Public"),
    n_private = sum(public_private == "Private"),
    urm_mean = mean(urm_prop),
    urm_sd = sd(urm_prop),
    urm_min = min(urm_prop),
    urm_max = max(urm_prop),
    legacy_mean = mean(legacy_mean),
    legacy_sd = sd(legacy_mean)
  )

table1 <- tibble(
  Variable = c(
    "Number of institutions",
    "  Public",
    "  Private",
    "",
    "URM enrollment share",
    "  Mean (SD)",
    "  Range",
    "",
    "Legacy emphasis (modal rating)",
    "  Does not consider (n)",
    "  Considers (n)",
    "",
    "Mean URM share by legacy emphasis",
    "  Does not consider",
    "  Considers",
    "  Difference"
  ),
  Value = c(
    as.character(overall_desc$n_schools),
    as.character(overall_desc$n_public),
    as.character(overall_desc$n_private),
    "",
    "",
    sprintf("%.1f%% (%.1f%%)", 100 * overall_desc$urm_mean, 100 * overall_desc$urm_sd),
    sprintf("%.1f%% - %.1f%%", 100 * overall_desc$urm_min, 100 * overall_desc$urm_max),
    "",
    "",
    as.character(legacy_dist$n[legacy_dist$considers_legacy == "Does not consider"]),
    as.character(legacy_dist$n[legacy_dist$considers_legacy == "Considers"]),
    "",
    "",
    sprintf("%.1f%%", 100 * urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Does not consider"]),
    sprintf("%.1f%%", 100 * urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Considers"]),
    sprintf("%.1f pp", 100 * (urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Does not consider"] -
                                urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Considers"]))
  )
)

write_csv(table1, file.path(OUT_DIR, "tables", "table1_descriptives.csv"))
cat("\nTable 1 saved.\n")

# ===============================================================================
# 3b) SECTOR × LEGACY DESCRIPTIVES (Supports interpretation/identification)
#   - Creates sector × legacy counts used later in key_numbers.
#   - Appendix Tables A1 and A2 are generated below.
# ===============================================================================

legacy_by_sector <- primary$school_data %>%
  count(public_private, considers_legacy, name = "n")

# ---------------------------------------------------------------------------
# Appendix Table A1 (Sector × Legacy) and Table A2 (Public-only descriptives)
#   - We write ONLY the appendix outputs we will actually use.
# ---------------------------------------------------------------------------

# Table A1: Sector × Legacy (counts + row %), primary sample
tabA1_sector_legacy <- primary$school_data %>%
  count(public_private, considers_legacy, name = "n") %>%
  mutate(considers_legacy = as.character(considers_legacy)) %>%
  tidyr::pivot_wider(
    names_from = considers_legacy,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    Total = `Does not consider` + Considers,
    `Does not consider` = sprintf("%d (%.0f%%)", `Does not consider`, 100 * `Does not consider` / Total),
    Considers = sprintf("%d (%.0f%%)", Considers, 100 * Considers / Total)
  ) %>%
  rename(Sector = public_private) %>%
  select(Sector, `Does not consider`, Considers, Total) %>%
  bind_rows({
    total_n <- nrow(primary$school_data)
    total_dn <- sum(primary$school_data$considers_legacy == "Does not consider")
    total_c  <- sum(primary$school_data$considers_legacy == "Considers")
    
    tibble(
      Sector = "Total",
      `Does not consider` = sprintf("%d (%.0f%%)", total_dn, 100 * total_dn / total_n),
      Considers = sprintf("%d (%.0f%%)", total_c, 100 * total_c / total_n),
      Total = total_n
    )
  })

write_csv(tabA1_sector_legacy, file.path(OUT_DIR, "appendix", "tableA1_sector_x_legacy.csv"))

# Table A2: URM by legacy emphasis (Public institutions only), primary sample
tabA2_public_only <- primary$school_data %>%
  filter(public_private == "Public") %>%
  group_by(considers_legacy) %>%
  summarise(
    n = n(),
    mean_urm = mean(urm_prop),
    sd_urm = sd(urm_prop),
    min_urm = min(urm_prop),
    max_urm = max(urm_prop),
    .groups = "drop"
  ) %>%
  mutate(
    mean_urm_pct = 100 * mean_urm,
    sd_urm_pp = 100 * sd_urm,
    min_urm_pct = 100 * min_urm,
    max_urm_pct = 100 * max_urm
  ) %>%
  select(considers_legacy, n, mean_urm_pct, sd_urm_pp, min_urm_pct, max_urm_pct)

write_csv(tabA2_public_only, file.path(OUT_DIR, "appendix", "tableA2_public_only_urm.csv"))

# Simple structural check (prints only; no stopping)
n_private_no_legacy <- legacy_by_sector %>%
  filter(public_private == "Private", considers_legacy == "Does not consider") %>%
  pull(n)
if (length(n_private_no_legacy) == 0) n_private_no_legacy <- 0L
if (n_private_no_legacy == 0) {
  cat("\nNOTE: In the primary sample, there are no Private institutions in the 'Does not consider' legacy category.\n")
}

# ===============================================================================
# 4) MAIN ANALYSIS: PRIMARY BINARY SPECIFICATION
# ===============================================================================

cat("\n=== MAIN ANALYSIS ===\n")

run_binary_model <- function(data, label) {
  
  form <- as.formula(paste(
    "urm_prop ~ considers_legacy + public_private +",
    paste(paste0("PC", 1:N_PCS), collapse = " + ")
  ))
  
  fit <- lm(form, data = data$school_data)
  coefs <- coef_table_hc(fit, type = SE_TYPE)
  
  legacy_coef <- coefs %>%
    filter(str_detect(term, "considers_legacy"))
  
  list(
    fit = fit,
    coefs = coefs,
    legacy_coef = legacy_coef,
    label = label,
    n = nrow(data$school_data),
    r2 = summary(fit)$r.squared
  )
}

primary_binary  <- run_binary_model(primary,  "Primary (n=36)")
balanced_binary <- run_binary_model(balanced, "Balanced (n=27)")

cat(sprintf("\nPrimary binary: %.2f pp (p = %.3f)\n",
            100 * primary_binary$legacy_coef$estimate,
            primary_binary$legacy_coef$p))
cat(sprintf("Balanced binary: %.2f pp (p = %.3f)\n",
            100 * balanced_binary$legacy_coef$estimate,
            balanced_binary$legacy_coef$p))

# Polychoric PCA robustness regression (primary sample, binary specification)
fit_poly <- lm(
  urm_prop ~ considers_legacy + public_private + PC1_poly + PC2_poly + PC3_poly,
  data = primary$school_data
)
poly_coefs <- coef_table_hc(fit_poly, type = SE_TYPE)
poly_legacy <- poly_coefs %>% filter(term == "considers_legacyConsiders")

# ===============================================================================
# 5) TABLE 2: MAIN RESULTS (Binary only)
# ===============================================================================

cat("\n=== TABLE 2: MAIN RESULTS ===\n")

table2 <- tibble(
  Specification = c(
    "Binary: Considers legacy (vs. does not)",
    "  Primary sample (n = 36)",
    "  Balanced sample (n = 27)"
  ),
  `Estimate (pp)` = c(
    "",
    sprintf("%.2f", 100 * primary_binary$legacy_coef$estimate),
    sprintf("%.2f", 100 * balanced_binary$legacy_coef$estimate)
  ),
  `95% CI` = c(
    "",
    sprintf("[%.2f, %.2f]", 100 * primary_binary$legacy_coef$ci_lower,
            100 * primary_binary$legacy_coef$ci_upper),
    sprintf("[%.2f, %.2f]", 100 * balanced_binary$legacy_coef$ci_lower,
            100 * balanced_binary$legacy_coef$ci_upper)
  ),
  `p-value` = c(
    "",
    sprintf("%.3f", primary_binary$legacy_coef$p),
    sprintf("%.3f", balanced_binary$legacy_coef$p)
  ),
  `R²` = c(
    "",
    sprintf("%.2f", primary_binary$r2),
    sprintf("%.2f", balanced_binary$r2)
  )
)

write_csv(table2, file.path(OUT_DIR, "tables", "table2_main_results.csv"))
print(table2)

# ===============================================================================
# 6) ROBUSTNESS CHECKS (Aligned to binary primary)
# ===============================================================================

cat("\n=== ROBUSTNESS CHECKS ===\n")

# --- 6a) PC sensitivity (PRIMARY spec: binary treatment) ---
max_pcs <- sum(str_detect(names(primary$school_data), "^PC\\d+$"))

pc_sensitivity_binary <- map_dfr(0:max_pcs, function(k) {
  if (k == 0) {
    form <- urm_prop ~ considers_legacy + public_private
  } else {
    form <- as.formula(paste(
      "urm_prop ~ considers_legacy + public_private +",
      paste(paste0("PC", 1:k), collapse = " + ")
    ))
  }
  fit <- lm(form, data = primary$school_data)
  coefs <- coef_table_hc(fit, type = SE_TYPE)
  legacy <- coefs %>% filter(str_detect(term, "considers_legacy"))
  
  tibble(
    n_pcs = k,
    estimate_pp = 100 * legacy$estimate,
    ci_lower_pp = 100 * legacy$ci_lower,
    ci_upper_pp = 100 * legacy$ci_upper,
    p = legacy$p
  )
})

cat("\nPC sensitivity (PRIMARY binary, primary sample):\n")
print(pc_sensitivity_binary)

# --- 6b) Leave-one-out (PRIMARY binary) ---
loo_results <- map_dfr(primary$school_data$school_name, function(s) {
  df_loo <- primary$school_data %>% filter(school_name != s)
  form <- as.formula(paste(
    "urm_prop ~ considers_legacy + public_private +",
    paste(paste0("PC", 1:N_PCS), collapse = " + ")
  ))
  fit <- lm(form, data = df_loo)
  coefs <- coef_table_hc(fit, type = SE_TYPE)
  legacy <- coefs %>% filter(str_detect(term, "considers_legacy"))
  
  tibble(
    left_out = s,
    estimate_pp = 100 * legacy$estimate,
    p = legacy$p
  )
})

loo_summary <- loo_results %>%
  summarise(
    min = min(estimate_pp),
    max = max(estimate_pp),
    mean = mean(estimate_pp),
    all_negative = all(estimate_pp < 0)
  )

cat("\nLeave-one-out summary (binary, primary):\n")
print(loo_summary)

# --- 6c) Fractional logit (binary; link-function robustness) ---
eps <- 1e-6
fit_frac <- glm(
  urm_prop ~ considers_legacy + public_private + PC1 + PC2 + PC3,
  data = primary$school_data %>% mutate(urm_prop = pmin(pmax(urm_prop, eps), 1 - eps)),
  family = quasibinomial(link = "logit")
)
beta_frac <- coef(fit_frac)[["considers_legacyConsiders"]]
p_bar <- mean(fitted(fit_frac))
ame_frac <- 100 * p_bar * (1 - p_bar) * beta_frac

cat(sprintf("Fractional logit AME: %.2f pp\n", ame_frac))

# --- 6d) Panel analysis (between vs within; supports cross-sectional averaging) ---
cat("\nPanel analysis...\n")

panel_data <- primary$year_data %>%
  mutate(
    legacy_z = as.numeric(scale(legacy_rating))
  )

var_decomp <- panel_data %>%
  group_by(school_name) %>%
  summarise(
    legacy_mean = mean(legacy_rating),
    legacy_sd = sd(legacy_rating),
    .groups = "drop"
  )

between_sd <- sd(var_decomp$legacy_mean)
within_sd <- mean(var_decomp$legacy_sd, na.rm = TRUE)

cat(sprintf("Between-school SD: %.3f\n", between_sd))
cat(sprintf("Mean within-school SD: %.3f\n", within_sd))

# Compute URM ICC from panel_data (between / (between + within))
urm_var_decomp <- panel_data %>%
  group_by(school_name) %>%
  summarise(
    urm_mean = mean(urm_prop, na.rm = TRUE),
    urm_var_within = var(urm_prop, na.rm = TRUE),
    .groups = "drop"
  )

urm_var_between <- var(urm_var_decomp$urm_mean, na.rm = TRUE)
urm_var_within_mean <- mean(urm_var_decomp$urm_var_within, na.rm = TRUE)

urm_icc <- urm_var_between / (urm_var_between + urm_var_within_mean)

cat(sprintf("URM ICC (computed): %.3f\n", urm_icc))

pdata <- pdata.frame(panel_data, index = c("school_name", "year_num"))
fit_between <- plm(urm_prop ~ legacy_z + public_private,
                   data = pdata, model = "between")
fit_within <- plm(urm_prop ~ legacy_z,
                  data = pdata, model = "within")

between_est <- 100 * coef(fit_between)["legacy_z"]
within_est <- 100 * coef(fit_within)["legacy_z"]

cat(sprintf("Between estimator: %.2f pp\n", between_est))
cat(sprintf("Within estimator: %.2f pp\n", within_est))

# ===============================================================================
# 7) TABLE 3: ROBUSTNESS SUMMARY (Binary-aligned)
# ===============================================================================

cat("\n=== TABLE 3: ROBUSTNESS SUMMARY ===\n")

table3 <- tibble(
  Check = c(
    "Primary specification (binary, 3 PCs)",
    "  Polychoric PCA controls (ordinal robustness)",
    "",
    "A. Sample robustness",
    "  Balanced 13-year panel",
    "",
    "B. Controls robustness (PRIMARY binary)",
    "  No PC controls",
    sprintf("  %d PC controls", max_pcs),
    "",
    "C. Link function",
    "  Fractional logit (AME)",
    "",
    "D. Influence",
    "  Leave-one-out range",
    "  All estimates negative",
    "",
    "E. Panel decomposition",
    "  Between-school estimator",
    "  Within-school estimator"
  ),
  `Estimate (pp)` = c(
    sprintf("%.2f", 100 * primary_binary$legacy_coef$estimate),
    sprintf("%.2f", 100 * poly_legacy$estimate),
    "",
    "",
    sprintf("%.2f", 100 * balanced_binary$legacy_coef$estimate),
    "",
    "",
    sprintf("%.2f", pc_sensitivity_binary$estimate_pp[pc_sensitivity_binary$n_pcs == 0]),
    sprintf("%.2f", pc_sensitivity_binary$estimate_pp[pc_sensitivity_binary$n_pcs == max_pcs]),
    "",
    "",
    sprintf("%.2f", ame_frac),
    "",
    "",
    sprintf("[%.2f, %.2f]", loo_summary$min, loo_summary$max),
    ifelse(loo_summary$all_negative, sprintf("Yes (%d/%d)", nrow(loo_results), nrow(loo_results)), "No"),
    "",
    "",
    sprintf("%.2f", between_est),
    sprintf("%.2f", within_est)
  ),
  `p-value / Note` = c(
    sprintf("%.3f", primary_binary$legacy_coef$p),
    sprintf("%.3f", poly_legacy$p),
    "",
    "",
    sprintf("%.3f", balanced_binary$legacy_coef$p),
    "",
    "",
    sprintf("%.3f", pc_sensitivity_binary$p[pc_sensitivity_binary$n_pcs == 0]),
    sprintf("%.3f", pc_sensitivity_binary$p[pc_sensitivity_binary$n_pcs == max_pcs]),
    "",
    "",
    "At-mean marginal effect",
    "",
    "",
    sprintf("Mean = %.2f pp", loo_summary$mean),
    "",
    "",
    "",
    sprintf("SE = %.2f", 100 * summary(fit_between)$coefficients["legacy_z", "Std. Error"]),
    sprintf("SE = %.2f", 100 * summary(fit_within)$coefficients["legacy_z", "Std. Error"])
  )
)

write_csv(table3, file.path(OUT_DIR, "tables", "table3_robustness.csv"))
print(table3, n = 30)

# ===============================================================================
# 8) FIGURE 1: URM BY LEGACY CATEGORY (Main text)
# ===============================================================================

cat("\n=== FIGURE 1 ===\n")

fig1_data <- primary$school_data %>%
  mutate(
    considers_legacy = factor(
      considers_legacy,
      levels = c("Does not consider", "Considers")
    )
  ) %>%
  group_by(considers_legacy) %>%
  summarise(
    mean_urm = mean(urm_prop),
    se_urm = sd(urm_prop) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = mean_urm - qt(0.975, n - 1) * se_urm,
    ci_upper = mean_urm + qt(0.975, n - 1) * se_urm,
    label = paste0(considers_legacy, "\n(n = ", n, ")")
  )

y_min <- 0
y_max <- min(1, max(fig1_data$ci_upper, na.rm = TRUE) + 0.01)

fig1 <- ggplot(fig1_data,
               aes(x = considers_legacy, y = mean_urm)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.12, linewidth = 0.6) +
  geom_point(size = 3.6) +
  scale_x_discrete(labels = fig1_data$label) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  labs(
    x = "Legacy emphasis in admissions",
    y = "Mean URM enrollment share"
  )

# Default ggplot theme. Sized for a single text column (~5.2 in wide) with a
# short aspect (not needlessly tall) so lettering prints at 8-12 pt; PDF is vector.
ggsave(file.path(OUT_DIR, "figures", "fig1_urm_by_legacy.png"),
       fig1, width = 5.2, height = 3.2, dpi = 600)
ggsave(file.path(OUT_DIR, "figures", "fig1_urm_by_legacy.pdf"),
       fig1, width = 5.2, height = 3.2)

cat("Figure 1 saved.\n")

# ===============================================================================
# APPENDIX OUTPUTS (FINAL)
#
# This section produces all appendix tables and figures referenced in the paper.
# Outputs are limited strictly to materials used in the Appendix; no unused or
# redundant files are generated.
#
# Appendix Tables:
#   Table A1. Sector × legacy policy (primary sample; counts with row percentages)
#   Table A2. URM enrollment descriptives within the public sector, by legacy policy
#   Table A3. Full regression coefficients: primary specification (HC2 SEs)
#   Table A4. Full regression coefficients: ordinal robustness using polychoric PCA
#   Table A5. Principal component loadings for non-legacy admissions priorities
#            (top five absolute loadings per component)
#   Table A6. Panel diagnostics summary (variance decomposition and estimators)
#
# Appendix Figures:
#   Figure A1. URM enrollment share versus continuous legacy emphasis (z-score),
#              by sector and binary legacy policy
#   Figure A2. Leave-one-out distribution of the legacy coefficient
#              (primary specification)
#
# All appendix outputs are written to OUT_DIR/appendix with filenames that match
# table and figure numbering exactly.
# ===============================================================================

# --- Table A3: Full model coefficients (primary binary baseline) ---
tableA3 <- primary_binary$coefs %>%
  mutate(
    estimate_pp = 100 * estimate,
    se_pp = 100 * se,
    ci = sprintf("[%.2f, %.2f]", 100 * ci_lower, 100 * ci_upper),
    p = sprintf("%.3f", p)
  ) %>%
  select(term, estimate_pp, se_pp, ci, p)

write_csv(tableA3, file.path(OUT_DIR, "appendix", "tableA3_full_coefficients_primary.csv"))

# --- Table A4: Full model coefficients (polychoric PCA controls; ordinal robustness) ---
tableA4 <- poly_coefs %>%
  mutate(
    estimate_pp = 100 * estimate,
    se_pp = 100 * se,
    ci = sprintf("[%.2f, %.2f]", 100 * ci_lower, 100 * ci_upper),
    p = sprintf("%.3f", p)
  ) %>%
  select(term, estimate_pp, se_pp, ci, p)

write_csv(tableA4, file.path(OUT_DIR, "appendix", "tableA4_full_coefficients_polychoric.csv"))

# --- Table A5: PC loadings (numeric PCA baseline): top five absolute loadings per PC ---
pc_loadings <- primary$pca_result$rotation[, 1:N_PCS, drop = FALSE] %>%
  as_tibble(rownames = "priority") %>%
  mutate(priority = str_remove(priority, "_z$"))

tableA5 <- map_dfr(1:N_PCS, function(i) {
  pc_col <- paste0("PC", i)
  pc_loadings %>%
    transmute(
      pc = pc_col,
      priority = priority,
      loading = .data[[pc_col]],
      abs_loading = abs(.data[[pc_col]])
    ) %>%
    arrange(desc(abs_loading)) %>%
    slice_head(n = 5) %>%
    mutate(
      loading = round(loading, 3)
    ) %>%
    select(pc, priority, loading)
})

write_csv(tableA5, file.path(OUT_DIR, "appendix", "tableA5_pc_loadings_top5.csv"))

var_explained <- summary(primary$pca_result)$importance[2, 1:N_PCS]
cum_var <- cumsum(var_explained)
cat(sprintf("Appendix note: PCs 1–%d explain %.1f%% of variance in other priorities (numeric PCA)\n",
            N_PCS, 100 * cum_var[N_PCS]))

# --- Figure A1: Scatter (legacy_z vs URM) with regression line ---
# IMPORTANT: remove manual colors to avoid implying substantive meaning via palette
figA1 <- ggplot(primary$school_data, aes(x = legacy_z, y = urm_prop)) +
  geom_point(aes(color = public_private, shape = considers_legacy),
             size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Legacy emphasis (z-score)",
    y = "URM enrollment share",
    color = "Sector",
    shape = "Legacy policy"
  ) +
  theme_gray(base_size = 11) +
  theme(legend.position = "bottom")

# Default ggplot theme; sized for the supplement column with vector output.
ggsave(file.path(OUT_DIR, "appendix", "figA1_scatter.png"), figA1,
       width = 5.4, height = 4.0, dpi = 600)
ggsave(file.path(OUT_DIR, "appendix", "figA1_scatter.pdf"), figA1,
       width = 5.4, height = 4.0)

# --- Figure A2: Leave-one-out distribution (binary) ---
figA2 <- ggplot(loo_results, aes(x = estimate_pp)) +
  geom_histogram(bins = 12, fill = "gray70", color = "white") +
  geom_vline(xintercept = 100 * primary_binary$legacy_coef$estimate,
             linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    x = "Legacy coefficient (percentage points)",
    y = "Count",
    caption = "Dashed line = full-sample estimate"
  ) +
  theme_gray(base_size = 11)

# Default ggplot theme; sized for the supplement column with vector output.
ggsave(file.path(OUT_DIR, "appendix", "figA2_leave_one_out.png"), figA2,
       width = 5.4, height = 3.6, dpi = 600)
ggsave(file.path(OUT_DIR, "appendix", "figA2_leave_one_out.pdf"), figA2,
       width = 5.4, height = 3.6)

# --- Table A6: Panel diagnostics summary ---
tableA6 <- tibble(
  Metric = c(
    "Institution-years",
    "Unique schools",
    "Between-school SD (legacy)",
    "Within-school SD (legacy, mean)",
    "Schools with any within-school variation",
    "ICC (URM variance)",
    "Between estimator",
    "Within estimator"
  ),
  Value = c(
    nrow(panel_data),
    n_distinct(panel_data$school_name),
    round(between_sd, 3),
    round(within_sd, 3),
    sum(var_decomp$legacy_sd > 0, na.rm = TRUE),
    sprintf("%.3f", urm_icc),
    sprintf("%.2f pp (SE = %.2f)", between_est,
            100 * summary(fit_between)$coefficients["legacy_z", "Std. Error"]),
    sprintf("%.2f pp (SE = %.2f)", within_est,
            100 * summary(fit_within)$coefficients["legacy_z", "Std. Error"])
  )
)

write_csv(tableA6, file.path(OUT_DIR, "appendix", "tableA6_panel_diagnostics.csv"))

# ===============================================================================
# 10) SAVE KEY NUMBERS FOR IN-TEXT CITATIONS
# ===============================================================================

# (Sector × legacy counts)
private_considers_n <- legacy_by_sector %>%
  filter(public_private == "Private", considers_legacy == "Considers") %>%
  pull(n)
if (length(private_considers_n) == 0) private_considers_n <- 0L

private_does_not_n <- legacy_by_sector %>%
  filter(public_private == "Private", considers_legacy == "Does not consider") %>%
  pull(n)
if (length(private_does_not_n) == 0) private_does_not_n <- 0L

public_considers_n <- legacy_by_sector %>%
  filter(public_private == "Public", considers_legacy == "Considers") %>%
  pull(n)
if (length(public_considers_n) == 0) public_considers_n <- 0L

public_does_not_n <- legacy_by_sector %>%
  filter(public_private == "Public", considers_legacy == "Does not consider") %>%
  pull(n)
if (length(public_does_not_n) == 0) public_does_not_n <- 0L

# (Public-only URM means by legacy)
public_urm_does_not_pct <- tabA2_public_only %>%
  filter(considers_legacy == "Does not consider") %>%
  pull(mean_urm_pct)
if (length(public_urm_does_not_pct) == 0) public_urm_does_not_pct <- NA_real_

public_urm_considers_pct <- tabA2_public_only %>%
  filter(considers_legacy == "Considers") %>%
  pull(mean_urm_pct)
if (length(public_urm_considers_pct) == 0) public_urm_considers_pct <- NA_real_

public_urm_diff_pp <- public_urm_does_not_pct - public_urm_considers_pct

key_numbers <- tibble(
  description = c(
    "n_schools_primary",
    "n_schools_balanced",
    "n_does_not_consider",
    "n_considers",
    "urm_does_not_consider_pct",
    "urm_considers_pct",
    "urm_difference_pp",
    "binary_estimate_pp",
    "binary_ci_lower_pp",
    "binary_ci_upper_pp",
    "binary_p",
    "balanced_binary_estimate_pp",
    "balanced_binary_p",
    "polychoric_binary_estimate_pp",
    "polychoric_binary_p",
    "pc0_binary_estimate_pp",
    "pc0_binary_p",
    "pcmax_binary_estimate_pp",
    "pcmax_binary_p",
    "loo_all_negative",
    "fraclogit_ame_pp",
    "between_sd",
    "within_sd",
    # new: sector × legacy structure
    "n_private_considers_legacy",
    "n_private_does_not_consider_legacy",
    "n_public_considers_legacy",
    "n_public_does_not_consider_legacy",
    # new: public-only URM descriptives by legacy
    "public_urm_does_not_consider_pct",
    "public_urm_considers_pct",
    "public_urm_difference_pp"
  ),
  value = c(
    primary$n_schools,
    balanced$n_schools,
    sum(primary$school_data$considers_legacy == "Does not consider"),
    sum(primary$school_data$considers_legacy == "Considers"),
    round(100 * urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Does not consider"], 1),
    round(100 * urm_by_legacy$mean_urm[urm_by_legacy$considers_legacy == "Considers"], 1),
    round(100 * (urm_by_legacy$mean_urm[1] - urm_by_legacy$mean_urm[2]), 1),
    round(100 * primary_binary$legacy_coef$estimate, 2),
    round(100 * primary_binary$legacy_coef$ci_lower, 2),
    round(100 * primary_binary$legacy_coef$ci_upper, 2),
    round(primary_binary$legacy_coef$p, 3),
    round(100 * balanced_binary$legacy_coef$estimate, 2),
    round(balanced_binary$legacy_coef$p, 3),
    round(100 * poly_legacy$estimate, 2),
    round(poly_legacy$p, 3),
    round(pc_sensitivity_binary$estimate_pp[pc_sensitivity_binary$n_pcs == 0], 2),
    round(pc_sensitivity_binary$p[pc_sensitivity_binary$n_pcs == 0], 3),
    round(pc_sensitivity_binary$estimate_pp[pc_sensitivity_binary$n_pcs == max_pcs], 2),
    round(pc_sensitivity_binary$p[pc_sensitivity_binary$n_pcs == max_pcs], 3),
    loo_summary$all_negative,
    round(ame_frac, 2),
    round(between_sd, 3),
    round(within_sd, 3),
    # new: sector × legacy structure
    private_considers_n,
    private_does_not_n,
    public_considers_n,
    public_does_not_n,
    # new: public-only URM descriptives by legacy
    round(public_urm_does_not_pct, 1),
    round(public_urm_considers_pct, 1),
    round(public_urm_diff_pp, 1)
  )
)

write_csv(key_numbers, file.path(OUT_DIR, "key_numbers_for_text.csv"))

# ===============================================================================
# FINAL SUMMARY
# ===============================================================================

cat("\n")
cat("==============================================================================\n")
cat("                         ANALYSIS COMPLETE                                    \n")
cat("==============================================================================\n")
cat("\n")
cat("OUTPUT DIRECTORY:", OUT_DIR, "\n")
cat("\n")
cat("MAIN TEXT:\n")
cat("  tables/table1_descriptives.csv\n")
cat("  tables/table2_main_results.csv\n")
cat("  tables/table3_robustness.csv\n")
cat("  figures/fig1_urm_by_legacy.png\n")
cat("\n")
cat("APPENDIX:\n")
cat("  appendix/tableA1_sector_x_legacy.csv\n")
cat("  appendix/tableA2_public_only_urm.csv\n")
cat("  appendix/tableA3_full_coefficients_primary.csv\n")
cat("  appendix/tableA4_full_coefficients_polychoric.csv\n")
cat("  appendix/tableA5_pc_loadings_top5.csv\n")
cat("  appendix/tableA6_panel_diagnostics.csv\n")
cat("  appendix/figA1_scatter.png\n")
cat("  appendix/figA2_leave_one_out.png\n")
cat("\n")
cat("KEY FINDING:\n")
cat(sprintf("  Institutions that consider legacy in admissions enroll %.1f pp fewer\n",
            abs(100 * primary_binary$legacy_coef$estimate)))
cat(sprintf("  URM students than those that do not (%.1f%% vs %.1f%%, p = %.3f).\n",
            100 * urm_by_legacy$mean_urm[2],
            100 * urm_by_legacy$mean_urm[1],
            primary_binary$legacy_coef$p))
cat("\n")
cat("ORDINAL ROBUSTNESS:\n")
cat(sprintf("  Polychoric PCA controls (binary): %.2f pp (p = %.3f)\n",
            100 * poly_legacy$estimate, poly_legacy$p))
cat("\n")
cat("==============================================================================\n")