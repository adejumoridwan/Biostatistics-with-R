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
km_fit <- survfit(surv_obj ~ 1, data = lung_clean)

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



# ==============================================================================
# 9. VISUALIZING COX MODEL RESULTS
# ==============================================================================

# 9.1 Forest Plot
# ---------------

ggforest(cox_multi, data = lung_complete,
         main = "Hazard Ratios from Multivariable Cox Model",
         fontsize = 0.8)


