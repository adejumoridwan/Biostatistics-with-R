# Load required packages
# If not installed, run: install.packages(c("survival", "survminer"))
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. LOAD AND EXPLORE THE DATA
# ==============================================================================

# We'll use the 'lung' dataset from the survival package
# This is a real dataset from the North Central Cancer Treatment Group
data(lung)

# View first few rows
head(lung)

# Check structure
str(lung)

# Summary statistics
summary(lung)

# Data description:
# inst: Institution code
# time: Survival time in days
# status: censoring status (1=censored, 2=dead)
# age: Age in years
# sex: Male=1, Female=2
# ph.ecog: ECOG performance score (0=good, 5=dead)
# ph.karno: Karnofsky performance score (0=bad, 100=good)
# pat.karno: Karnofsky performance score rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months

# Check for missing data
colSums(is.na(lung))

# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

# Create a working dataset
lung_clean <- lung %>%
  # Recode status: survival package expects 0=censored, 1=event
  # Original data has 1=censored, 2=dead, so we subtract 1
  mutate(
    death = status - 1,  # 0 = censored, 1 = dead
    sex_f = factor(sex, levels = 1:2, labels = c("Male", "Female")),
    ph.ecog_f = factor(ph.ecog),
    age_group = cut(age, breaks = c(0, 60, 70, 100), 
                    labels = c("≤60", "61-70", ">70"))
  ) %>%
  # Remove rows with missing key variables
  filter(!is.na(time), !is.na(death), !is.na(sex))

# Check the cleaned data
dim(lung_clean)
summary(lung_clean)

# ==============================================================================
# 3. CREATE SURVIVAL OBJECT
# ==============================================================================

# The Surv() function creates a survival object
# Format: Surv(time, event)
surv_obj <- Surv(time = lung_clean$time, event = lung_clean$death)

# View first few survival objects
head(surv_obj)
# "+" indicates censored observations

# Summary of survival object
summary(surv_obj)

# ==============================================================================
# 4. KAPLAN-MEIER SURVIVAL CURVES
# ==============================================================================

# 4.1 Overall Survival Curve
# --------------------------

# Fit Kaplan-Meier survival curve
km_fit <- survfit(Surv(time, death) ~ 1, data = lung_clean)

# Print summary
print(km_fit)

# Detailed summary at specific time points
summary(km_fit, times = c(0, 100, 200, 300, 400, 500, 600, 700))

# Median survival time with confidence interval
print(km_fit)

# Basic plot
plot(km_fit, 
     xlab = "Time (days)", 
     ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curve - Lung Cancer",
     conf.int = TRUE)

# Enhanced plot using survminer
ggsurvplot(
  km_fit,
  data = lung_clean,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  ggtheme = theme_minimal(),
  palette = "#2E9FDF",
  title = "Overall Survival - Lung Cancer Patients",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  break.time.by = 100,
  legend = "none"
)

# 4.2 Survival by Sex
# -------------------

# Fit KM curves stratified by sex
km_sex <- survfit(Surv(time, death) ~ sex_f, data = lung_clean)

# Print summary
print(km_sex)

# Summary at specific times
summary(km_sex, times = c(100, 200, 300, 400, 500))

# Enhanced plot
ggsurvplot(
  km_sex,
  data = lung_clean,
  conf.int = TRUE,
  pval = TRUE,              # Add log-rank p-value
  risk.table = TRUE,
  risk.table.col = "strata",
  legend.labs = c("Male", "Female"),
  legend.title = "Sex",
  palette = c("#E7B800", "#2E9FDF"),
  title = "Survival by Sex",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  break.time.by = 100,
  ggtheme = theme_minimal()
)

# 4.3 Survival by Age Group
# --------------------------

# Remove missing age groups
lung_age <- lung_clean %>% filter(!is.na(age_group))

# Fit KM curves stratified by age group
km_age <- survfit(Surv(time, death) ~ age_group, data = lung_age)

# Print summary
print(km_age)

# Enhanced plot
ggsurvplot(
  km_age,
  data = lung_age,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  legend.labs = c("≤60 years", "61-70 years", ">70 years"),
  legend.title = "Age Group",
  palette = c("#00BA38", "#619CFF", "#F8766D"),
  title = "Survival by Age Group",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  break.time.by = 100,
  ggtheme = theme_minimal()
)



# ==============================================================================
# 5. LOG-RANK TEST
# ==============================================================================

# 5.1 Compare Survival by Sex
# ----------------------------

# Perform log-rank test
logrank_sex <- survdiff(Surv(time, death) ~ sex_f, data = lung_clean)

# Print results
print(logrank_sex)

# Interpretation:
# - Chi-square statistic
# - Degrees of freedom
# - P-value
# - Observed vs Expected events in each group

# 5.2 Compare Survival by Age Group
# ----------------------------------

logrank_age <- survdiff(Surv(time, death) ~ age_group, data = lung_age)
print(logrank_age)


# 5.4 Pairwise Comparisons
# -------------------------

# For age groups, perform pairwise log-rank tests
pairwise_survdiff(Surv(time, death) ~ age_group, 
                  data = lung_age,
                  p.adjust.method = "bonferroni")

# ==============================================================================
# 6. COX PROPORTIONAL HAZARDS MODEL
# ==============================================================================

# 6.1 Univariable Cox Models
# ---------------------------

# Sex
cox_sex <- coxph(Surv(time, death) ~ sex_f, data = lung_clean)
summary(cox_sex)

# Age (continuous)
cox_age <- coxph(Surv(time, death) ~ age, data = lung_clean)
summary(cox_age)

# ECOG performance score
cox_ecog <- coxph(Surv(time, death) ~ ph.ecog, data = lung_clean)
summary(cox_ecog)

# Extract and display hazard ratios with CIs
# For sex
exp(coef(cox_sex))  # Hazard ratio
exp(confint(cox_sex))  # 95% CI

# 6.2 Multivariable Cox Model
# ----------------------------

# Prepare data without missing values for key variables
lung_complete <- lung_clean %>%
  filter(!is.na(age), !is.na(sex_f), !is.na(ph.ecog), !is.na(ph.karno))

# Fit multivariable model
cox_multi <- coxph(Surv(time, death) ~ age + sex_f + ph.ecog + ph.karno, 
                   data = lung_complete)

# Full summary
summary(cox_multi)

# Key components in summary:
# - Coefficients (log hazard ratios)
# - Hazard ratios (exp(coef))
# - 95% confidence intervals
# - P-values
# - Concordance index (C-index)
# - Likelihood ratio test, Wald test, Score test

# Extract hazard ratios and 95% CIs
hr_table <- data.frame(
  Variable = names(coef(cox_multi)),
  HR = exp(coef(cox_multi)),
  Lower_CI = exp(confint(cox_multi))[,1],
  Upper_CI = exp(confint(cox_multi))[,2],
  P_value = summary(cox_multi)$coefficients[,5]
)
print(hr_table)

# 6.3 Model with Interaction
# ---------------------------

# Test interaction between sex and ECOG score
cox_interaction <- coxph(Surv(time, death) ~ age + sex_f * ph.ecog + ph.karno, 
                         data = lung_complete)
summary(cox_interaction)

# Compare models using likelihood ratio test
anova(cox_multi, cox_interaction)

# ==============================================================================
# 7. INTERPRETING HAZARD RATIOS
# ==============================================================================

# Extract and interpret key hazard ratios from multivariable model

# 7.1 Sex Effect
# --------------
hr_sex <- exp(coef(cox_multi)["sex_fFemale"])
ci_sex <- exp(confint(cox_multi)["sex_fFemale",])

hr_sex
ci_sex

cat("\nSex (Female vs Male):\n")
cat("Hazard Ratio:", round(hr_sex, 3), "\n")
cat("95% CI:", round(ci_sex[1], 3), "-", round(ci_sex[2], 3), "\n")
cat("Interpretation: Females have", round((1-hr_sex)*100, 1), 
    "% lower hazard of death compared to males.\n")

# 7.2 Age Effect
# --------------
hr_age <- exp(coef(cox_multi)["age"])
hr_age_10 <- exp(coef(cox_multi)["age"] * 10)

cat("\nAge (per year):\n")
cat("Hazard Ratio:", round(hr_age, 3), "\n")
cat("Per 10 years:", round(hr_age_10, 3), "\n")
cat("Interpretation: Each additional year of age increases hazard by", 
    round((hr_age-1)*100, 2), "%.\n")
cat("A 10-year age difference corresponds to", 
    round((hr_age_10-1)*100, 1), "% increase in hazard.\n")

# 7.3 ECOG Score Effect
# ----------------------
hr_ecog <- exp(coef(cox_multi)["ph.ecog"])

cat("\nECOG Performance Score (per unit increase):\n")
cat("Hazard Ratio:", round(hr_ecog, 3), "\n")
cat("Interpretation: Each unit increase in ECOG score (worse performance)",
    "\nincreases hazard by", round((hr_ecog-1)*100, 1), "%.\n")

# ==============================================================================
# 8. MODEL DIAGNOSTICS
# ==============================================================================

# 8.1 Test Proportional Hazards Assumption
# -----------------------------------------

# Global test and individual tests for each covariate
ph_test <- cox.zph(cox_multi)
print(ph_test)

# Plot Schoenfeld residuals

# use ggcoxzph from survminer
ggcoxzph(ph_test)

# 8.2 Check Influential Observations
# -----------------------------------

# Plot dfbeta values (change in coefficient if observation removed)
ggcoxdiagnostics(cox_multi, type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_minimal())

# 8.3 Check Linearity for Continuous Variables
# ---------------------------------------------

# Martingale residuals for age
ggcoxfunctional(Surv(time, death) ~ age + log(age) + sqrt(age), 
                data = lung_complete)

# ==============================================================================
# 9. VISUALIZING COX MODEL RESULTS
# ==============================================================================

# 9.1 Forest Plot
# ---------------

ggforest(cox_multi, data = lung_complete,
         main = "Hazard Ratios from Multivariable Cox Model",
         fontsize = 0.8)

# 9.2 Survival Curves from Cox Model
# -----------------------------------

# Plot adjusted survival curves for males vs females
# (adjusted for mean values of other covariates)
cox_sex_adjusted <- coxph(Surv(time, death) ~ sex_f + age + ph.ecog + ph.karno,
                          data = lung_complete)

# Create data for plotting
new_data <- data.frame(
  sex_f = factor(c("Male", "Female"), levels = c("Male", "Female")),
  age = rep(mean(lung_complete$age, na.rm = TRUE), 2),
  ph.ecog = rep(mean(lung_complete$ph.ecog, na.rm = TRUE), 2),
  ph.karno = rep(mean(lung_complete$ph.karno, na.rm = TRUE), 2)
)

# Predict survival curves
fit_adjusted <- survfit(cox_sex_adjusted, newdata = new_data)

# Plot
ggsurvplot(fit_adjusted, 
           data = lung_complete,
           conf.int = TRUE,
           legend.labs = c("Male (adjusted)", "Female (adjusted)"),
           legend.title = "Sex",
           palette = c("#E7B800", "#2E9FDF"),
           title = "Adjusted Survival Curves by Sex",
           subtitle = "Adjusted for age, ECOG, and Karnofsky score",
           xlab = "Time (days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())

# ==============================================================================
# 10. ADDITIONAL ANALYSES
# ==============================================================================

# 10.1 Stratified Cox Model
# --------------------------

# If proportional hazards violated for a variable, stratify by it
# Example: stratify by institution
cox_stratified <- coxph(Surv(time, death) ~ age + sex_f + ph.ecog + 
                          strata(inst), 
                        data = lung_complete)
summary(cox_stratified)

# 10.2 Time-Dependent Coefficients
# ---------------------------------

# If proportional hazards violated, can model time-varying effect
# Example: interaction with time for sex
cox_time_varying <- coxph(Surv(time, death) ~ age + sex_f + ph.ecog + 
                            ph.karno + sex_f:time,
                          data = lung_complete)
summary(cox_time_varying)

# 10.3 Prediction
# ---------------

# Calculate risk scores for new patients
new_patients <- data.frame(
  age = c(50, 65, 75),
  sex_f = factor(c("Male", "Female", "Male"), levels = c("Male", "Female")),
  ph.ecog = c(0, 1, 2),
  ph.karno = c(90, 80, 70)
)

# Predict risk scores (linear predictor)
risk_scores <- predict(cox_multi, newdata = new_patients, type = "lp")
cat("\nRisk Scores (higher = worse prognosis):\n")
print(data.frame(new_patients, Risk_Score = risk_scores))

# Predict survival probabilities at specific times
surv_probs <- summary(survfit(cox_multi, newdata = new_patients), 
                      times = c(180, 365))
cat("\nPredicted Survival Probabilities:\n")
print(surv_probs)

# ==============================================================================
# 11. REPORTING RESULTS
# ==============================================================================

# Create a comprehensive results table
results_table <- data.frame(
  Characteristic = c("Age (per year)", "Sex (Female vs Male)", 
                     "ECOG Score", "Karnofsky Score"),
  N_Events = c(nrow(lung_complete), nrow(lung_complete), 
               nrow(lung_complete), nrow(lung_complete)),
  HR = round(exp(coef(cox_multi)), 3),
  CI_Lower = round(exp(confint(cox_multi))[,1], 3),
  CI_Upper = round(exp(confint(cox_multi))[,2], 3),
  P_Value = round(summary(cox_multi)$coefficients[,5], 4)
)

# Format for publication
results_table$HR_CI <- paste0(results_table$HR, " (", 
                              results_table$CI_Lower, "-", 
                              results_table$CI_Upper, ")")

# Final table
final_table <- results_table[, c("Characteristic", "HR_CI", "P_Value")]
print(final_table)

# ==============================================================================
# 12. SUMMARY STATISTICS
# ==============================================================================

cat("Total patients:", nrow(lung_complete), "\n")
cat("Total deaths:", sum(lung_complete$death), "\n")
cat("Median follow-up (days):", median(lung_complete$time), "\n")
cat("Median survival (days):", 
    survfit(Surv(time, death) ~ 1, data = lung_complete)$median, "\n")

# By sex
cat("\n--- By Sex ---\n")
lung_complete %>%
  group_by(sex_f) %>%
  summarise(
    N = n(),
    Deaths = sum(death),
    Median_Survival = median(time),
    Mean_Age = round(mean(age), 1)
  ) %>%
  print()

