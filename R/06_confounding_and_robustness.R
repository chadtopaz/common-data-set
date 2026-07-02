# ===============================================================================
# 06_referee_revision_analysis.R
#
# Referee-revision analyses (replication package, R).
# Mirrors R/05_analysis.R idioms exactly (janitor::clean_names, prcomp,
# sandwich::vcovHC HC2, t-based inference) so results reconcile with the
# published pipeline. Validated in parallel against a Python implementation
# (an independent Python implementation): primary -7.81 pp, balanced -6.47 pp
# reproduce to the penny.
#
# Produces:
#   - baseline reproduction (sanity check)
#   - CONFOUNDING SENSITIVITY: legacy coef adding hand-coded institutional
#     covariates (state affirmative-action ban, flagship, region), a few at a time
#   - pre-COVID (2010-2019) sensitivity
#   - ordinal focal-variable robustness (0-3 rating vs binary)
#   - sample-flow + included-vs-excluded composition
#   - PCA loadings / eigenvalues
#   - three-group descriptive time-trend figure
#
# Outputs -> output/
# ===============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sandwich)
})

set.seed(42)
options(dplyr.summarise.inform = FALSE)

INPUT_FILE        <- "data/Final_2010_2022.csv"
MIN_YEARS_PRIMARY <- 8
MIN_YEARS_BAL     <- 13
N_PCS             <- 3
SE_TYPE           <- "HC2"
OUT_DIR           <- "output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

# ---- HC2 robust coefficient table with t-based inference (verbatim from 05) ----
coef_table_hc <- function(model, type = "HC2") {
  V <- sandwich::vcovHC(model, type = type)
  est <- coef(model); se <- sqrt(diag(V))
  t_stat <- est / se; df <- df.residual(model)
  p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  crit <- qt(0.975, df = df)
  tibble(term = names(est), estimate = as.numeric(est), se = as.numeric(se),
         t = as.numeric(t_stat), df = df, p = as.numeric(p_val),
         ci_lower = estimate - crit * se, ci_upper = estimate + crit * se)
}

# ============================ LOAD + COLUMN SETUP ==============================
data_raw_all <- read_csv(INPUT_FILE, show_col_types = FALSE) %>% clean_names()
if ("level_of_applicant_s_interest" %in% names(data_raw_all) &&
    !"level_of_applicants_interest" %in% names(data_raw_all)) {
  data_raw_all <- rename(data_raw_all, level_of_applicants_interest = level_of_applicant_s_interest)
}

legacy_var <- "alumni_ae_relation"
priority_cols_all <- c(
  "rigor_of_secondary_school_record", "class_rank", "academic_gpa",
  "standardized_test_scores", "application_essay", "recommendation_s",
  "interview", "extracurricular_activities", "talent_ability",
  "character_personal_qualities", "first_generation", "alumni_ae_relation",
  "geographical_residence", "state_residency", "religious_affiliation_commitment",
  "racial_ethnic_status", "volunteer_work", "work_experience",
  "level_of_applicants_interest")
priority_cols   <- intersect(priority_cols_all, names(data_raw_all))
other_priorities <- setdiff(priority_cols, legacy_var)
fy_cols  <- names(data_raw_all)[str_detect(names(data_raw_all), "^degree_seeking_first_year_")]
urm_cols <- intersect(c("degree_seeking_first_year_aian", "degree_seeking_first_year_black",
                        "degree_seeking_first_year_latinx", "degree_seeking_first_year_nhpi"),
                      names(data_raw_all))

# ============================ SCHOOL-LEVEL BUILDER =============================
# Mirrors create_school_data() in 05_analysis.R (polychoric branch omitted).
create_school_data <- function(data_all, min_years) {
  data_filtered <- data_all %>% group_by(school_name) %>%
    filter(n() >= min_years) %>% ungroup()

  year_data <- data_filtered %>%
    mutate(legacy_rating = to_num(.data[[legacy_var]]),
           across(all_of(fy_cols), to_num),
           across(all_of(priority_cols), to_num),
           year_num = as.numeric(str_extract(as.character(year), "^\\d{4}"))) %>%
    mutate(total_fy  = rowSums(across(all_of(fy_cols)),  na.rm = TRUE),
           urm_count = rowSums(across(all_of(urm_cols)), na.rm = TRUE),
           urm_prop  = ifelse(total_fy > 0, urm_count / total_fy, NA_real_)) %>%
    filter(!is.na(legacy_rating), !is.na(urm_prop), total_fy > 0)

  school_data <- year_data %>% group_by(school_name) %>%
    summarise(legacy_mean  = mean(legacy_rating, na.rm = TRUE),
              legacy_modal = as.numeric(names(sort(table(legacy_rating), decreasing = TRUE))[1]),
              urm_prop     = mean(urm_prop, na.rm = TRUE),
              across(all_of(other_priorities), ~ mean(.x, na.rm = TRUE)),
              public_private = first(public_private),
              n_years = n(), .groups = "drop") %>%
    filter(is.finite(legacy_mean), is.finite(urm_prop), !is.na(public_private)) %>%
    mutate(considers_legacy = factor(legacy_modal >= 2, levels = c(FALSE, TRUE),
                                     labels = c("Does not consider", "Considers")),
           legacy_modal_0_3 = legacy_modal - 1,
           legacy_mean_0_3  = legacy_mean  - 1)

  school_scaled <- school_data %>%
    mutate(across(all_of(other_priorities), ~ as.numeric(scale(.x)), .names = "{.col}_z"))
  X_other <- school_scaled %>% dplyr::select(ends_with("_z")) %>% as.matrix()
  pca_result <- prcomp(X_other, center = FALSE, scale. = FALSE)
  n_pcs <- min(6, ncol(pca_result$x))
  pc_scores <- pca_result$x[, 1:n_pcs, drop = FALSE]
  colnames(pc_scores) <- paste0("PC", 1:n_pcs)
  school_final <- bind_cols(school_scaled, as_tibble(pc_scores))

  list(school_data = school_final, year_data = year_data,
       pca_result = pca_result, n_schools = nrow(school_final))
}

extract_legacy <- function(fit, term = "considers_legacyConsiders", type = SE_TYPE) {
  ct <- coef_table_hc(fit, type) %>% filter(term == !!term)
  c(pp = 100 * ct$estimate, lo = 100 * ct$ci_lower, hi = 100 * ct$ci_upper,
    p = ct$p, r2 = summary(fit)$r.squared)
}

primary  <- create_school_data(data_raw_all, MIN_YEARS_PRIMARY)
balanced <- create_school_data(data_raw_all, MIN_YEARS_BAL)

# ============================ (0) BASELINE CHECK ==============================
base_form <- as.formula(paste("urm_prop ~ considers_legacy + public_private +",
                              paste(paste0("PC", 1:N_PCS), collapse = " + ")))
cat("=== Baseline reproduction (expect -7.81 / -6.47) ===\n")
for (nm in c("primary", "balanced")) {
  d <- get(nm); fit <- lm(base_form, data = d$school_data)
  L <- extract_legacy(fit)
  cat(sprintf("  %-9s n=%d  legacy=%+.2f pp  CI [%.2f, %.2f]  p=%.3f  R2=%.2f\n",
              nm, d$n_schools, L["pp"], L["lo"], L["hi"], L["p"], L["r2"]))
}

# ============================ COVARIATES =====================================
# Single source of truth: data/covariates.csv (a PROVIDED
# data file, coding + sources documented in 01_covariates.py). Read rather than
# duplicated inline so the R and Python covariate coding cannot diverge.
#   aa_restriction          = PRIMARY (formal statutory/administrative restriction
#                             CA/MI/WA/FL; Georgia EXCLUDED as court-based)
#   aa_restriction_incl_ga  = sensitivity variant adding Georgia (court-based)
#   flagship                = primary OR land-grant flagship (Purdue/Texas A&M = 1); exploratory
covariates <- read_csv(file.path("data", "covariates.csv"),
                       show_col_types = FALSE) %>%
  dplyr::select(school_name, state, region, flagship, aa_restriction,
                ga_court_based, aa_restriction_incl_ga)

stopifnot(all(primary$school_data$school_name %in% covariates$school_name))

# ============================ (1) CONFOUNDING SENSITIVITY =====================
sd_cov <- primary$school_data %>% left_join(covariates, by = "school_name") %>%
  mutate(region = factor(region, levels = c("Northeast", "Midwest", "South", "West")))

specs <- list(
  "A. Baseline (paper primary)"                = "considers_legacy + public_private + PC1 + PC2 + PC3",
  "B. + AA-restriction (formal, excl GA)"      = "considers_legacy + public_private + PC1 + PC2 + PC3 + aa_restriction",
  "C. + AA-restriction incl. GA (court-based)" = "considers_legacy + public_private + PC1 + PC2 + PC3 + aa_restriction_incl_ga",
  "D. + flagship (exploratory)"                = "considers_legacy + public_private + PC1 + PC2 + PC3 + flagship",
  "E. + region"                                = "considers_legacy + public_private + PC1 + PC2 + PC3 + region",
  "F. + AA-restriction + flagship"             = "considers_legacy + public_private + PC1 + PC2 + PC3 + aa_restriction + flagship",
  "G. + AA-restriction + flagship + region"    = "considers_legacy + public_private + PC1 + PC2 + PC3 + aa_restriction + flagship + region"
)
cat("\n=== Confounding sensitivity (legacy coefficient, pp; HC2) ===\n")
own_rows <- list()
conf_rows <- map_dfr(names(specs), function(nm) {
  fit <- lm(as.formula(paste("urm_prop ~", specs[[nm]])), data = sd_cov)
  L <- extract_legacy(fit)
  cat(sprintf("  %-44s %+6.2f  [%6.2f, %6.2f]  p=%.3f  R2=%.2f\n",
              nm, L["pp"], L["lo"], L["hi"], L["p"], L["r2"]))
  # covariate own-coefficients (mirror Python out_covariate_own_coefs.csv)
  ct <- coef_table_hc(fit, SE_TYPE)
  for (cvar in c("aa_restriction", "aa_restriction_incl_ga", "flagship")) {
    if (cvar %in% ct$term) {
      row <- ct %>% filter(term == cvar)
      own_rows[[length(own_rows) + 1]] <<- tibble(spec = nm, covariate = cvar,
        coef_pp = round(100 * row$estimate, 2), p = round(row$p, 3))
    }
  }
  tibble(spec = nm, legacy_pp = round(L["pp"], 2), ci_lo = round(L["lo"], 2),
         ci_hi = round(L["hi"], 2), p = round(L["p"], 3),
         n_params = length(coef(fit)), r2 = round(L["r2"], 3))
})
write_csv(bind_rows(own_rows), file.path(OUT_DIR, "R_covariate_own_coefs.csv"))

# HC3 conservative sensitivity on baseline and full saturated model
cat("\n=== HC3 sensitivity (conservative) ===\n")
hc3_specs <- list("A. Baseline [HC3]" = specs[["A. Baseline (paper primary)"]],
                  "G. Full saturated [HC3]" = specs[["G. + AA-restriction + flagship + region"]])
hc3_rows <- map_dfr(names(hc3_specs), function(nm) {
  fit <- lm(as.formula(paste("urm_prop ~", hc3_specs[[nm]])), data = sd_cov)
  L <- extract_legacy(fit, type = "HC3")
  cat(sprintf("  %-24s %+6.2f  [%6.2f, %6.2f]  p=%.3f\n", nm, L["pp"], L["lo"], L["hi"], L["p"]))
  tibble(spec = nm, legacy_pp = round(L["pp"], 2), ci_lo = round(L["lo"], 2),
         ci_hi = round(L["hi"], 2), p = round(L["p"], 3),
         n_params = length(coef(fit)), r2 = round(summary(fit)$r.squared, 3))
})
write_csv(bind_rows(conf_rows, hc3_rows), file.path(OUT_DIR, "R_confounding_sensitivity.csv"))

# within-public legacy x AA-restriction cross-tabs (save BOTH)
cat("\nWithin-public legacy x AA-restriction (primary, excl GA):\n")
x1 <- sd_cov %>% filter(public_private == "Public") %>% count(considers_legacy, aa_restriction)
print(x1)
cat("Within-public legacy x AA-restriction incl. GA:\n")
x2 <- sd_cov %>% filter(public_private == "Public") %>% count(considers_legacy, aa_restriction_incl_ga)
print(x2)
ct_long <- bind_rows(
  x1 %>% rename(restriction = aa_restriction) %>% mutate(variable = "AA-restriction (primary, excl GA)"),
  x2 %>% rename(restriction = aa_restriction_incl_ga) %>% mutate(variable = "AA-restriction incl. GA")
) %>% dplyr::select(variable, considers_legacy, restriction, n)
write_csv(ct_long, file.path(OUT_DIR, "R_aa_crosstab_public.csv"))

# ============================ (2) PRE-COVID SENSITIVITY =======================
# Restrict to entering classes <= cutoff; require >=4 USABLE years (rows with
# non-missing legacy & computable URM), recomputing PCA on the retained window.
cat("\n=== Pre-COVID / period sensitivity ===\n")
primary_names <- primary$school_data$school_name
pre_rows <- map_dfr(list(c("Full 2010-2022", "2022"),
                         c("Pre-COVID 2010-2019 entering", "2019")), function(yr) {
  d_sub <- data_raw_all %>%
    mutate(year_num = as.numeric(str_extract(as.character(year), "^\\d{4}"))) %>%
    filter(school_name %in% primary_names, year_num <= as.numeric(yr[[2]]))
  keep <- create_school_data(d_sub, 1)$year_data %>% count(school_name) %>%
    filter(n >= 4) %>% pull(school_name)
  dd <- create_school_data(d_sub %>% filter(school_name %in% keep), 1)
  fit <- lm(base_form, data = dd$school_data); L <- extract_legacy(fit)
  cat(sprintf("  %-30s n=%d  legacy=%+.2f pp  CI [%.2f, %.2f]  p=%.3f  R2=%.2f\n",
              yr[[1]], dd$n_schools, L["pp"], L["lo"], L["hi"], L["p"], L["r2"]))
  tibble(window = yr[[1]], n = dd$n_schools, legacy_pp = round(L["pp"], 2),
         ci_lo = round(L["lo"], 2), ci_hi = round(L["hi"], 2), p = round(L["p"], 3),
         r2 = round(L["r2"], 3))
})
write_csv(pre_rows, file.path(OUT_DIR, "R_precovid.csv"))

# ============================ (3) ORDINAL FOCAL ROBUSTNESS ====================
# Robustness coding only, NOT dose-response (upper categories sparse: 8/27/1/0).
cat("\n=== Ordinal focal legacy measure (primary sample) ===\n")
ord_labels <- c(considers_legacy = "binary (paper primary)",
                legacy_modal_0_3 = "modal ordinal 0-3 (per step)",
                legacy_mean_0_3  = "mean ordinal 0-3 (per step)")
ord_rows <- map_dfr(names(ord_labels), function(tm) {
  f <- as.formula(paste("urm_prop ~", tm, "+ public_private + PC1 + PC2 + PC3"))
  fit <- lm(f, data = primary$school_data)
  trm <- if (tm == "considers_legacy") "considers_legacyConsiders" else tm
  ct <- coef_table_hc(fit, SE_TYPE) %>% filter(term == !!trm)
  cat(sprintf("  %-30s %+.2f pp  CI [%.2f, %.2f]  p=%.3f\n",
              ord_labels[[tm]], 100*ct$estimate, 100*ct$ci_lower, 100*ct$ci_upper, ct$p))
  tibble(measure = ord_labels[[tm]], est_pp = round(100*ct$estimate, 2),
         ci_lo = round(100*ct$ci_lower, 2), ci_hi = round(100*ct$ci_upper, 2), p = round(ct$p, 3))
})
write_csv(ord_rows, file.path(OUT_DIR, "R_ordinal.csv"))
cat("Legacy modal (0-3) distribution:\n"); print(table(primary$school_data$legacy_modal_0_3))
tibble(legacy_modal_0_3 = as.integer(names(table(primary$school_data$legacy_modal_0_3))),
       n_schools = as.integer(table(primary$school_data$legacy_modal_0_3))) %>%
  write_csv(file.path(OUT_DIR, "R_legacy_modal_distribution.csv"))

# ============================ (4) SAMPLE FLOW ================================
cat("\n=== Sample flow ===\n")
panel_all <- create_school_data(data_raw_all, 1)
sec <- data_raw_all %>% group_by(school_name) %>% summarise(s = first(public_private))
excl <- setdiff(panel_all$school_data$school_name, primary_names)
flow <- tibble(
  stage = c("Forbes top-50 (from paper)", "Analytic-window panel", "Primary (>=8y)",
            "Balanced (13y)", "Excluded from primary"),
  n = c(50L, panel_all$n_schools, primary$n_schools, balanced$n_schools, length(excl)),
  note = c("25 public + 25 private",
           "schools with any usable year",
           sprintf("%d public / %d private",
                   sum(sec$s[sec$school_name %in% primary_names] == "Public"),
                   sum(sec$s[sec$school_name %in% primary_names] == "Private")),
           "complete reporting",
           sprintf("%d public / %d private",
                   sum(sec$s[sec$school_name %in% excl] == "Public"),
                   sum(sec$s[sec$school_name %in% excl] == "Private"))))
print(flow)
write_csv(flow, file.path(OUT_DIR, "R_sample_flow.csv"))

# ============================ (5) PCA LOADINGS / EIGENVALUES ==================
pca <- primary$pca_result
eig <- pca$sdev^2
varprop <- eig / sum(eig)
cat("\n=== PCA of non-legacy priorities ===\n")
for (i in 1:6) cat(sprintf("  PC%d  var%%=%.1f  cum%%=%.1f  eigenvalue=%.2f\n",
                           i, 100*varprop[i], 100*cumsum(varprop)[i], eig[i]))
eig_tbl <- tibble(PC = paste0("PC", 1:6), var_pct = round(100*varprop[1:6], 1),
                  cum_pct = round(100*cumsum(varprop)[1:6], 1), eigenvalue = round(eig[1:6], 2))
write_csv(eig_tbl, file.path(OUT_DIR, "R_pca_eigenvalues.csv"))
load_tbl <- as_tibble(pca$rotation[, 1:3], rownames = "priority")
write_csv(load_tbl, file.path(OUT_DIR, "R_pca_loadings.csv"))

# ============================ (6) THREE-GROUP TREND FIGURE ====================
group_lookup <- primary$school_data %>%
  transmute(school_name,
            group3 = case_when(
              considers_legacy == "Does not consider" ~ "Public / non-legacy",
              public_private == "Public"              ~ "Public / legacy",
              TRUE                                    ~ "Private / legacy"))
trend <- primary$year_data %>%
  left_join(group_lookup, by = "school_name") %>%
  group_by(year_num, group3) %>%
  summarise(urm = 100 * mean(urm_prop, na.rm = TRUE),
            n_schools = n_distinct(school_name), .groups = "drop")
# Distinguish the three groups by colour, POINT SHAPE, and LINE TYPE so the
# figure remains legible without colour (journal accessibility requirement).
# No internal plot title (the title belongs in the caption only).
p <- ggplot(trend, aes(year_num, urm, color = group3, shape = group3, linetype = group3)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.4) +
  scale_color_manual(values = c("Public / non-legacy" = "#1b7837",
                                "Public / legacy" = "#762a83",
                                "Private / legacy" = "#b35806")) +
  scale_linetype_manual(values = c("Public / non-legacy" = "solid",
                                   "Public / legacy" = "longdash",
                                   "Private / legacy" = "dotted")) +
  scale_shape_manual(values = c("Public / non-legacy" = 16,
                                "Public / legacy" = 17,
                                "Private / legacy" = 15)) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  labs(x = "Academic year (fall)", y = "Mean URM first-year share (%)",
       color = NULL, shape = NULL, linetype = NULL) +
  theme_gray(base_size = 11) +
  theme(legend.position = "bottom")
# Sized for a single text column (~5.2 in wide): embedded at column width the
# lettering prints at 8-12 pt. PDF is vector (preferred by the journal).
ggsave(file.path(OUT_DIR, "R_fig_urm_trend_3group.png"), p, width = 5.2, height = 3.9, dpi = 600)
# device = cairo_pdf embeds fonts in the vector PDF (journal requirement)
ggsave(file.path(OUT_DIR, "R_fig_urm_trend_3group.pdf"), p, width = 5.2, height = 3.9, device = cairo_pdf)
write_csv(trend, file.path(OUT_DIR, "R_trend_3group.csv"))

cat("\nDone. Outputs in", OUT_DIR, "\n")
