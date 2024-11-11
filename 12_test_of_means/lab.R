# Medical Statistical Analysis: T-tests and ANOVA
# Using NCCTG Lung Cancer Dataset

# Load required libraries
library(survival)  # For lung cancer dataset
library(tidyverse)
library(ggplot2)
library(car)  # For Levene's test

# Load the lung cancer dataset
data(lung)

# Data preparation
# Convert categorical variables
lung$sex_label <- factor(lung$sex, labels = c("Male", "Female"))
lung$status_label <- factor(lung$status, labels = c("Censored", "Dead"))

# 1. ONE SAMPLE T-TEST
# Normality Check for Age
# Histogram
ggplot(lung, aes(x = age)) +
  geom_histogram(bins = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Patient Age")

# Shapiro-Wilk Test for Normality of Age
shapiro_test_age <- shapiro.test(lung$age)
print("Shapiro-Wilk Test for Age Normality:")
print(shapiro_test_age)


# Test if the mean age is different from a hypothetical population age of 60
t_test_one_sample <- t.test(lung$age, mu = 60)
print("One Sample T-Test (Age):")
print(t_test_one_sample)



# 2. INDEPENDENT TWO-SAMPLE T-TEST
# Compare survival times between males and females

# Levene's Test for Homogeneity of Variance
levene_test_survival <- leveneTest(
  lung$time, 
  lung$sex_label
)
print("Levene's Test for Homogeneity of Variance:")
print(levene_test_survival)

t_test_survival_by_sex <- t.test(
  time ~ sex_label, 
  data = lung
)
print("Independent Two-Sample T-Test (Survival Time by Sex):")
print(t_test_survival_by_sex)

# 3. ANOVA 
# Create age groups for ANOVA
lung$age_group <- cut(
  lung$age, 
  breaks = c(0, 50, 60, 70, Inf), 
  labels = c("Young", "Middle-Aged", "Senior", "Elderly")
)

# Perform One-Way ANOVA on survival time across age groups
anova_model <- aov(
  time ~ age_group, 
  data = lung
)
print("One-Way ANOVA Results (Survival Time by Age Group):")
print(summary(anova_model))

# Post-hoc Tukey Test
tukey_test <- TukeyHSD(anova_model)
print("Tukey Post-hoc Test:")
print(tukey_test)

# Visualization of Survival Time by Age Group
ggplot(lung, aes(x = age_group, y = time)) +
  geom_boxplot() +
  labs(
    title = "Survival Time by Age Group", 
    x = "Age Group", 
    y = "Survival Time"
  ) +
  theme_minimal()

# Diagnostic Plot for ANOVA
# Q-Q Plot to check normality of residuals
plot(anova_model, which = 2)


# Additional Descriptive Statistics
summary_stats <- lung %>%
  group_by(sex_label) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_survival_time = mean(time, na.rm = TRUE),
    sd_survival_time = sd(time, na.rm = TRUE)
  )
print("Summary Statistics by Sex:")
print(summary_stats)