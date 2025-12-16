# ============================================================================
# Lecture 108: Non-Parametric Tests Lab
# Practical Biostatistics with R
# ============================================================================

# Load required packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

# ============================================================================
# PART 1: CHECKING NORMALITY AND DECIDING ON TEST TYPE
# ============================================================================

# Example dataset: Blood pressure measurements
set.seed(123)
bp_data <- data.frame(
  patient_id = 1:20,
  before = rnorm(20, mean = 140, sd = 15),
  after = rnorm(20, mean = 130, sd = 15)
)

# Calculate differences
bp_data$difference <- bp_data$after - bp_data$before

# Visual assessment of normality
# 1. Histogram
ggplot(bp_data, aes(x = difference)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black") +
  labs(title = "Distribution of BP Differences",
       x = "Difference (After - Before)",
       y = "Frequency") +
  theme_minimal()

# 2. Q-Q plot
ggqqplot(bp_data$difference, 
         title = "Q-Q Plot of BP Differences",
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles")

# 3. Boxplot
ggplot(bp_data, aes(y = difference)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of BP Differences",
       y = "Difference (After - Before)") +
  theme_minimal()

# Statistical tests for normality
# Shapiro-Wilk test (best for small samples)
shapiro.test(bp_data$difference)

# Interpretation:
# If p > 0.05: Data is likely normal → use parametric test
# If p < 0.05: Data is not normal → use non-parametric test


# ============================================================================
# PART 2: WILCOXON SIGNED RANK TEST
# ============================================================================

# Scenario: Paired blood pressure measurements (before and after treatment)
# Research question: Does treatment reduce blood pressure?

# View the data
head(bp_data)

# Descriptive statistics
summary(bp_data[, c("before", "after")])

# Calculate medians and IQR
median(bp_data$before)
IQR(bp_data$before)
median(bp_data$after)
IQR(bp_data$after)

# Perform Wilcoxon Signed Rank Test
# Two-sided test
wilcox_result <- wilcox.test(bp_data$after, bp_data$before, 
                             paired = TRUE,
                             alternative = "two.sided",
                             conf.int = TRUE)
print(wilcox_result)

# One-sided test (testing if after < before, i.e., reduction)
wilcox_result_one <- wilcox.test(bp_data$after, bp_data$before, 
                                 paired = TRUE,
                                 alternative = "less",
                                 conf.int = TRUE)
print(wilcox_result_one)

# Visualize paired data
bp_long <- bp_data %>%
  pivot_longer(cols = c(before, after), 
               names_to = "timepoint", 
               values_to = "bp")

ggpaired(bp_long, x = "timepoint", y = "bp",
         color = "timepoint", line.color = "gray",
         palette = c("#00AFBB", "#FC4E07")) +
  labs(title = "Blood Pressure Before and After Treatment",
       y = "Blood Pressure (mmHg)")

# Effect size (r = Z / sqrt(N))
# Extract Z statistic manually or use wilcoxonZ
library(rstatix)
wilcox_effsize <- wilcox_effsize(bp_long, bp ~ timepoint, paired = TRUE)
print(wilcox_effsize)


# ============================================================================
# PART 3: MANN-WHITNEY U TEST
# ============================================================================

# Scenario: Compare recovery time between two treatment groups
set.seed(456)
recovery_data <- data.frame(
  treatment = rep(c("Drug_A", "Drug_B"), each = 25),
  recovery_days = c(rexp(25, rate = 0.15),  # Drug A - right skewed
                    rexp(25, rate = 0.10))   # Drug B - right skewed
)

# Descriptive statistics by group
recovery_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    median = median(recovery_days),
    IQR = IQR(recovery_days),
    min = min(recovery_days),
    max = max(recovery_days)
  )

# Visualize the distributions
ggplot(recovery_data, aes(x = treatment, y = recovery_days, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Recovery Time by Treatment",
       x = "Treatment",
       y = "Recovery Time (days)") +
  theme_minimal()

# Check normality by group
recovery_data %>%
  group_by(treatment) %>%
  summarise(
    shapiro_p = shapiro.test(recovery_days)$p.value
  )

# Perform Mann-Whitney U Test
mann_whitney_result <- wilcox.test(recovery_days ~ treatment, 
                                   data = recovery_data,
                                   alternative = "two.sided",
                                   conf.int = TRUE)
print(mann_whitney_result)

# One-sided test (if Drug_A < Drug_B)
mann_whitney_one <- wilcox.test(recovery_days ~ treatment, 
                                data = recovery_data,
                                alternative = "less",
                                conf.int = TRUE)
print(mann_whitney_one)

# Effect size
mann_effsize <- wilcox_effsize(recovery_days ~ treatment, 
                               data = recovery_data)
print(mann_effsize)

# Violin plot with boxplot overlay
ggplot(recovery_data, aes(x = treatment, y = recovery_days, fill = treatment)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  labs(title = "Recovery Time Distribution by Treatment",
       x = "Treatment",
       y = "Recovery Time (days)") +
  theme_minimal()


# ============================================================================
# PART 4: KRUSKAL-WALLIS TEST
# ============================================================================

# Scenario: Compare pain scores across 4 different drug doses
set.seed(789)
pain_data <- data.frame(
  dose = rep(c("Placebo", "Low", "Medium", "High"), each = 20),
  pain_score = c(
    sample(5:10, 20, replace = TRUE),  # Placebo
    sample(4:9, 20, replace = TRUE),   # Low
    sample(3:7, 20, replace = TRUE),   # Medium
    sample(1:5, 20, replace = TRUE)    # High
  )
)

# Make dose an ordered factor
pain_data$dose <- factor(pain_data$dose, 
                         levels = c("Placebo", "Low", "Medium", "High"),
                         ordered = TRUE)

# Descriptive statistics
pain_data %>%
  group_by(dose) %>%
  summarise(
    n = n(),
    median = median(pain_score),
    IQR = IQR(pain_score),
    mean = mean(pain_score),
    sd = sd(pain_score)
  )

# Visualize
ggplot(pain_data, aes(x = dose, y = pain_score, fill = dose)) +
  geom_boxplot() +
  labs(title = "Pain Scores by Dose Level",
       x = "Dose",
       y = "Pain Score (0-10)") +
  theme_minimal()

# Perform Kruskal-Wallis Test
kruskal_result <- kruskal.test(pain_score ~ dose, data = pain_data)
print(kruskal_result)

# Effect size (Epsilon squared)
library(rstatix)
kruskal_effsize <- kruskal_effsize(pain_score ~ dose, data = pain_data)
print(kruskal_effsize)

# Post-hoc pairwise comparisons (if Kruskal-Wallis is significant)
# Dunn's test with Bonferroni correction
dunn_result <- dunn_test(pain_score ~ dose, 
                         data = pain_data,
                         p.adjust.method = "bonferroni")
print(dunn_result)

# Visualize with significance levels
# Create a plot with pairwise comparisons
pwc <- dunn_result %>%
  filter(p.adj < 0.05)  # Only significant comparisons

ggplot(pain_data, aes(x = dose, y = pain_score, fill = dose)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label.y = 11) +
  labs(title = "Pain Scores by Dose with Kruskal-Wallis Test",
       x = "Dose",
       y = "Pain Score (0-10)") +
  theme_minimal()


# ============================================================================
# PART 5: SPEARMAN RANK CORRELATION
# ============================================================================

# Scenario: Relationship between study hours and exam rank
set.seed(321)
study_data <- data.frame(
  student_id = 1:30,
  study_hours = rpois(30, lambda = 15) + rnorm(30, 0, 2),
  exam_rank = sample(1:30, 30, replace = FALSE)
)

# Add some monotonic non-linear relationship
study_data$exam_rank <- 31 - rank(study_data$study_hours^1.5 + rnorm(30, 0, 3))

# View the data
head(study_data)

# Descriptive statistics
summary(study_data[, c("study_hours", "exam_rank")])

# Visualize the relationship
ggplot(study_data, aes(x = study_hours, y = exam_rank)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Relationship between Study Hours and Exam Rank",
       x = "Study Hours per Week",
       y = "Exam Rank (1 = Best)") +
  theme_minimal()

# Perform Spearman Correlation
spearman_result <- cor.test(study_data$study_hours, 
                            study_data$exam_rank,
                            method = "spearman",
                            exact = FALSE)  # Use normal approximation
print(spearman_result)

# Compare with Pearson (for demonstration)
pearson_result <- cor.test(study_data$study_hours, 
                           study_data$exam_rank,
                           method = "pearson")
print(pearson_result)

# Calculate both correlations
cor(study_data$study_hours, study_data$exam_rank, method = "spearman")
cor(study_data$study_hours, study_data$exam_rank, method = "pearson")

# Correlation matrix visualization
library(corrplot)
cor_matrix <- cor(study_data[, c("study_hours", "exam_rank")], 
                  method = "spearman")
corrplot(cor_matrix, method = "number", type = "upper",
         title = "Spearman Correlation Matrix",
         mar = c(0,0,2,0))

# Scatterplot with both correlation values
ggplot(study_data, aes(x = study_hours, y = exam_rank)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = sprintf("Study Hours vs Exam Rank\nSpearman ρ = %.3f, Pearson r = %.3f",
                       cor(study_data$study_hours, study_data$exam_rank, method = "spearman"),
                       cor(study_data$study_hours, study_data$exam_rank, method = "pearson")),
       x = "Study Hours per Week",
       y = "Exam Rank (1 = Best)") +
  theme_minimal()


# ============================================================================
# PART 6: COMPREHENSIVE EXAMPLE WITH REAL-WORLD SCENARIO
# ============================================================================

# Scenario: Clinical trial comparing 3 diet interventions on weight loss
set.seed(999)
diet_trial <- data.frame(
  participant_id = 1:75,
  diet = rep(c("Control", "Mediterranean", "Low_Carb"), each = 25),
  baseline_weight = rnorm(75, mean = 85, sd = 12),
  week12_weight = NA,
  age = sample(25:65, 75, replace = TRUE),
  baseline_bmi = rnorm(75, mean = 30, sd = 4)
)

# Simulate weight loss (non-normal, different effects)
diet_trial$week12_weight <- diet_trial$baseline_weight - 
  c(rexp(25, 0.5),      # Control - small loss
    rexp(25, 0.7),      # Mediterranean - moderate loss
    rexp(25, 0.9))      # Low Carb - larger loss

diet_trial$weight_change <- diet_trial$week12_weight - diet_trial$baseline_weight

# Make diet a factor
diet_trial$diet <- factor(diet_trial$diet, 
                          levels = c("Control", "Mediterranean", "Low_Carb"))

# 1. Check normality of weight change by group
normality_check <- diet_trial %>%
  group_by(diet) %>%
  summarise(
    shapiro_p = shapiro.test(weight_change)$p.value,
    normal = ifelse(shapiro_p > 0.05, "Yes", "No")
  )
print(normality_check)

# 2. Descriptive statistics
descriptives <- diet_trial %>%
  group_by(diet) %>%
  summarise(
    n = n(),
    median_change = median(weight_change),
    IQR_change = IQR(weight_change),
    mean_change = mean(weight_change),
    sd_change = sd(weight_change)
  )
print(descriptives)

# 3. Visualize distributions
ggplot(diet_trial, aes(x = diet, y = weight_change, fill = diet)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Weight Change by Diet Intervention",
       x = "Diet Group",
       y = "Weight Change (kg)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# 4. Kruskal-Wallis test
kw_diet <- kruskal.test(weight_change ~ diet, data = diet_trial)
print(kw_diet)

# 5. Post-hoc Dunn's test
dunn_diet <- dunn_test(weight_change ~ diet, 
                       data = diet_trial,
                       p.adjust.method = "bonferroni")
print(dunn_diet)

# 6. Spearman correlation: Age vs Weight Change
cor_age_weight <- cor.test(diet_trial$age, 
                           diet_trial$weight_change,
                           method = "spearman")
print(cor_age_weight)

# Visualize correlation
ggplot(diet_trial, aes(x = age, y = weight_change)) +
  geom_point(aes(color = diet), size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = sprintf("Age vs Weight Change\nSpearman ρ = %.3f, p = %.4f",
                       cor(diet_trial$age, diet_trial$weight_change, 
                           method = "spearman"),
                       cor_age_weight$p.value),
       x = "Age (years)",
       y = "Weight Change (kg)") +
  theme_minimal()

# 7. Spearman correlation: Baseline BMI vs Weight Change
cor_bmi_weight <- cor.test(diet_trial$baseline_bmi, 
                           diet_trial$weight_change,
                           method = "spearman")
print(cor_bmi_weight)


# ============================================================================
# PART 7: REPORTING TEMPLATE
# ============================================================================

# Function to create a summary report
create_nonparam_report <- function(test_result, test_name) {
  cat("\n", rep("=", 60), "\n")
  cat("RESULTS:", test_name, "\n")
  cat(rep("=", 60), "\n")
  print(test_result)
  cat("\nInterpretation:\n")
  if(test_result$p.value < 0.05) {
    cat("- Result is statistically significant (p < 0.05)\n")
    cat("- We reject the null hypothesis\n")
  } else {
    cat("- Result is not statistically significant (p ≥ 0.05)\n")
    cat("- We fail to reject the null hypothesis\n")
  }
  cat(rep("=", 60), "\n\n")
}

# Example usage
create_nonparam_report(wilcox_result, "Wilcoxon Signed Rank Test")
create_nonparam_report(mann_whitney_result, "Mann-Whitney U Test")


# ============================================================================
# PART 8: PRACTICE EXERCISES
# ============================================================================

# Exercise 1: Create your own dataset and perform Wilcoxon test
# Hint: Measure something before and after an intervention

# Exercise 2: Compare two independent groups using Mann-Whitney
# Hint: Compare outcomes between males and females

# Exercise 3: Compare 4+ groups using Kruskal-Wallis
# Hint: Compare satisfaction scores across different departments

# Exercise 4: Calculate Spearman correlation for two variables
# Hint: Correlate ordinal variables like education level and income bracket

# Exercise 5: Create a complete analysis workflow
# - Check normality
# - Choose appropriate test
# - Perform the test
# - Calculate effect size
# - Visualize results
# - Write interpretation


# ============================================================================
# SUMMARY: QUICK REFERENCE GUIDE
# ============================================================================

# 1. Wilcoxon Signed Rank Test (Paired samples)
# wilcox.test(x, y, paired = TRUE)

# 2. Mann-Whitney U Test (Two independent groups)
# wilcox.test(outcome ~ group, data = df)

# 3. Kruskal-Wallis Test (3+ independent groups)
# kruskal.test(outcome ~ group, data = df)
# Post-hoc: dunn_test(outcome ~ group, data = df, p.adjust.method = "bonferroni")

# 4. Spearman Correlation
# cor.test(x, y, method = "spearman")

# Always report:
# - Test statistic
# - P-value
# - Median (IQR) for groups
# - Effect size when available
# - Confidence intervals if provided

cat("\n✓ Lab complete! You've learned all major non-parametric tests.\n")