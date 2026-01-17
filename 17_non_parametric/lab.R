# Load required packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(coin)

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


