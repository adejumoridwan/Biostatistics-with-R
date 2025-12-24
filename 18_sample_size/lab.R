# ============================================================================
# Sample Size & Power Analysis - R Code
# Practical Biostatistics with R
# ============================================================================

# Install and load required packages
# install.packages("pwr")
# install.packages("ggplot2")

library(pwr)
library(ggplot2)

# ============================================================================
# Why Sample Size is Important
# ============================================================================

# Example: Calculating effect size from pilot data
# Scenario: New drug to lower blood pressure

# Pilot data
treatment_mean <- 120  # mmHg
control_mean <- 130    # mmHg
pooled_sd <- 15        # mmHg

# Calculate Cohen's d (effect size)
cohens_d <- (treatment_mean - control_mean) / pooled_sd
cohens_d

# if cohens_d < 0.3 then the effect size is small, but if cohens_d < 0.7 then medium, else large


# ============================================================================
# Types of Errors & Power
# ============================================================================

# Demonstrating the relationship between alpha, beta, and power
alpha <- 0.05  # Type I error rate
beta <- 0.20   # Type II error rate
power <- 1 - beta

alpha
beta
power


# Visualizing power with different sample sizes
effect_size <- 0.5
sample_sizes <- seq(10, 200, by = 5)

# Calculate power for each sample size
power_values <- sapply(sample_sizes, function(n) {
  result <- pwr.t.test(n = n, d = effect_size, sig.level = 0.05, 
                       type = "two.sample", alternative = "two.sided")
  return(result$power)
})

# Create power curve
power_data <- data.frame(n = sample_sizes, power = power_values)

ggplot(power_data, aes(x = n, y = power)) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red") +
  labs(title = "Power Curve for Two-Sample t-test",
       subtitle = "Effect size d = 0.5, α = 0.05",
       x = "Sample Size per Group",
       y = "Statistical Power") +
  theme_minimal() +
  annotate("text", x = 150, y = 0.82, label = "80% Power", color = "red")

# ============================================================================
# Power Analysis in R (pwr package)
# ============================================================================

# ---------------------------------------------------------------------------
# Basic power analysis: Calculate sample size
# ---------------------------------------------------------------------------

# Two-sample t-test: How many participants do we need?
power_analysis_1 <- pwr.t.test(
  d = 0.5,              # Medium effect size
  sig.level = 0.05,     # Alpha level
  power = 0.80,         # Desired power
  type = "two.sample",  # Independent samples
  alternative = "two.sided"
)

print(power_analysis_1)

# Extract sample size needed per group
n_per_group <- ceiling(power_analysis_1$n)
cat("\nSample size needed per group:", n_per_group, "\n")
cat("Total sample size:", n_per_group * 2, "\n")

# ---------------------------------------------------------------------------
# Calculate power for a given sample size
# ---------------------------------------------------------------------------

power_analysis_2 <- pwr.t.test(
  n = 50,               # Sample size per group
  d = 0.5,              # Effect size
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

cat("\nWith n=50 per group, power =", round(power_analysis_2$power, 3), "\n")

# ---------------------------------------------------------------------------
# Calculate detectable effect size for given n and power
# ---------------------------------------------------------------------------

power_analysis_3 <- pwr.t.test(
  n = 30,               # Available sample size
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided"
)

cat("\nWith n=30 per group and 80% power, detectable effect size d =", 
    round(power_analysis_3$d, 3), "\n")

# ---------------------------------------------------------------------------
# Creating comprehensive power curves for multiple effect sizes
# ---------------------------------------------------------------------------

# Sample sizes to evaluate
sample_sizes <- seq(10, 200, by = 2)

# Multiple effect sizes
effect_sizes <- c(0.2, 0.5, 0.8)

# Calculate power for all combinations
power_curves <- data.frame()

for (d in effect_sizes) {
  powers <- sapply(sample_sizes, function(n) {
    result <- pwr.t.test(n = n, d = d, sig.level = 0.05,
                         type = "two.sample", alternative = "two.sided")
    return(result$power)
  })
  
  temp_df <- data.frame(
    n = sample_sizes,
    power = powers,
    effect_size = paste("d =", d)
  )
  
  power_curves <- rbind(power_curves, temp_df)
}

# Plot comprehensive power curves
ggplot(power_curves, aes(x = n, y = power, color = effect_size)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray40") +
  labs(title = "Power Curves for Different Effect Sizes",
       subtitle = "Two-sample t-test, α = 0.05",
       x = "Sample Size per Group",
       y = "Statistical Power",
       color = "Effect Size") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +
  annotate("text", x = 180, y = 0.82, label = "80% Power", color = "gray40")

# ---------------------------------------------------------------------------
# Sensitivity analysis: What if effect size is uncertain?
# ---------------------------------------------------------------------------

# Expected effect size
expected_d <- 0.5

# Calculate sample size for expected effect
main_analysis <- pwr.t.test(d = expected_d, sig.level = 0.05, 
                            power = 0.80, type = "two.sample")
n_expected <- ceiling(main_analysis$n)

cat("\n=== Sensitivity Analysis ===\n")
cat("Expected effect (d = 0.5): n =", n_expected, "per group\n")

# What if effect is smaller?
smaller_effect <- pwr.t.test(n = n_expected, d = 0.3, sig.level = 0.05,
                             type = "two.sample")
cat("If true effect is d = 0.3: power =", round(smaller_effect$power, 3), "\n")

# What if effect is larger?
larger_effect <- pwr.t.test(n = n_expected, d = 0.7, sig.level = 0.05,
                            type = "two.sample")
cat("If true effect is d = 0.7: power =", round(larger_effect$power, 3), "\n")

# ============================================================================
# Determining Sample Size for t-tests
# ============================================================================

# ---------------------------------------------------------------------------
# Independent Samples t-test
# ---------------------------------------------------------------------------

cat("\n=== INDEPENDENT SAMPLES T-TEST ===\n")

# Example: Comparing treatment vs control groups
# Treatment mean = 25, Control mean = 20, SD = 10

mean_treatment <- 25
mean_control <- 20
sd_pooled <- 10

d_independent <- (mean_treatment - mean_control) / sd_pooled

# Two-sided test
independent_two_sided <- pwr.t.test(
  d = d_independent,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided"
)

cat("Two-sided test:\n")
cat("  Effect size d =", d_independent, "\n")
cat("  Sample size per group:", ceiling(independent_two_sided$n), "\n")
cat("  Total sample size:", ceiling(independent_two_sided$n) * 2, "\n\n")

# One-sided test (if we predict direction)
independent_one_sided <- pwr.t.test(
  d = d_independent,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "greater"
)

cat("One-sided test:\n")
cat("  Sample size per group:", ceiling(independent_one_sided$n), "\n")
cat("  Total sample size:", ceiling(independent_one_sided$n) * 2, "\n\n")

# ---------------------------------------------------------------------------
# Paired Samples t-test
# ---------------------------------------------------------------------------

cat("=== PAIRED SAMPLES T-TEST ===\n")

# Example: Before-after blood pressure measurement
# Mean difference = 5 mmHg, SD of differences = 8 mmHg

mean_diff <- 5
sd_diff <- 8

d_paired <- mean_diff / sd_diff

paired_analysis <- pwr.t.test(
  d = d_paired,
  sig.level = 0.05,
  power = 0.80,
  type = "paired",
  alternative = "two.sided"
)

cat("Effect size d =", d_paired, "\n")
cat("Number of pairs needed:", ceiling(paired_analysis$n), "\n\n")

# Compare paired vs independent design
cat("Comparison: Paired vs Independent Design\n")
cat("Paired design needs:", ceiling(paired_analysis$n), "pairs\n")
cat("Independent design needs:", ceiling(independent_two_sided$n), 
    "per group =", ceiling(independent_two_sided$n) * 2, "total\n")
cat("Efficiency gain:", 
    round((ceiling(independent_two_sided$n) * 2 - ceiling(paired_analysis$n)) / 
            (ceiling(independent_two_sided$n) * 2) * 100, 1), 
    "% fewer participants\n\n")

# ---------------------------------------------------------------------------
# One-sample t-test
# ---------------------------------------------------------------------------

cat("=== ONE-SAMPLE T-TEST ===\n")

# Example: Is average IQ of medical students different from 100?
# Hypothesized mean = 105, SD = 15, Population mean = 100

hypothesized_mean <- 105
population_mean <- 100
sd_population <- 15

d_one_sample <- (hypothesized_mean - population_mean) / sd_population

one_sample_analysis <- pwr.t.test(
  d = d_one_sample,
  sig.level = 0.05,
  power = 0.80,
  type = "one.sample",
  alternative = "two.sided"
)

cat("Testing if μ ≠", population_mean, "\n")
cat("Effect size d =", d_one_sample, "\n")
cat("Sample size needed:", ceiling(one_sample_analysis$n), "\n\n")

# ---------------------------------------------------------------------------
# Sample Size Table for Different Effect Sizes
# ---------------------------------------------------------------------------

cat("=== SAMPLE SIZE REFERENCE TABLE ===\n")
cat("Two-sample t-test, α = 0.05, power = 0.80\n\n")

effect_sizes <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0)
sample_size_table <- data.frame(
  Effect_Size = effect_sizes,
  n_per_group = numeric(length(effect_sizes)),
  Total_N = numeric(length(effect_sizes))
)

for (i in 1:length(effect_sizes)) {
  result <- pwr.t.test(d = effect_sizes[i], sig.level = 0.05, 
                       power = 0.80, type = "two.sample")
  sample_size_table$n_per_group[i] <- ceiling(result$n)
  sample_size_table$Total_N[i] <- ceiling(result$n) * 2
}

print(sample_size_table)

# ---------------------------------------------------------------------------
# Accounting for Attrition
# ---------------------------------------------------------------------------

cat("\n=== ACCOUNTING FOR ATTRITION ===\n")

required_n <- 64  # From power analysis
attrition_rate <- 0.20  # 20% expected dropout

recruited_n <- ceiling(required_n / (1 - attrition_rate))

cat("Required sample size (from power analysis):", required_n, "\n")
cat("Expected attrition rate:", attrition_rate * 100, "%\n")
cat("Recruit this many participants:", recruited_n, "\n")
cat("Buffer added:", recruited_n - required_n, "participants\n")

# ============================================================================
# Sample Size for Proportion Tests
# ============================================================================

# ---------------------------------------------------------------------------
# Two-Proportion Test
# ---------------------------------------------------------------------------

cat("\n=== TWO-PROPORTION TEST ===\n")

# Example: Comparing vaccination efficacy
# Group 1 (vaccine): 80% success rate
# Group 2 (placebo): 60% success rate

p1 <- 0.80
p2 <- 0.60

# Calculate Cohen's h
h <- ES.h(p1, p2)

cat("Proportion 1:", p1, "\n")
cat("Proportion 2:", p2, "\n")
cat("Difference:", p1 - p2, "\n")
cat("Cohen's h =", round(h, 3), "\n\n")

# Power analysis for two proportions
two_prop_analysis <- pwr.2p.test(
  h = h,
  sig.level = 0.05,
  power = 0.80,
  alternative = "two.sided"
)

cat("Sample size per group:", ceiling(two_prop_analysis$n), "\n")
cat("Total sample size:", ceiling(two_prop_analysis$n) * 2, "\n\n")

# ---------------------------------------------------------------------------
# One-Proportion Test
# ---------------------------------------------------------------------------

cat("=== ONE-PROPORTION TEST ===\n")

# Example: Is local vaccination rate different from national rate?
# National rate: 70%
# Expected local rate: 65%

p_null <- 0.70
p_alternative <- 0.65

h_one_prop <- ES.h(p_alternative, p_null)

one_prop_analysis <- pwr.p.test(
  h = h_one_prop,
  sig.level = 0.05,
  power = 0.80,
  alternative = "two.sided"
)

cat("Null hypothesis proportion:", p_null, "\n")
cat("Alternative proportion:", p_alternative, "\n")
cat("Cohen's h =", round(h_one_prop, 3), "\n")
cat("Sample size needed:", ceiling(one_prop_analysis$n), "\n\n")

# ---------------------------------------------------------------------------
# Sample Size Table for Proportion Tests
# ---------------------------------------------------------------------------

cat("=== PROPORTION TEST REFERENCE TABLE ===\n")
cat("Two-proportion test, α = 0.05, power = 0.80\n\n")

p2_fixed <- 0.50  # Baseline proportion
differences <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

prop_table <- data.frame(
  p1 = p2_fixed + differences,
  p2 = p2_fixed,
  Difference = differences,
  Cohen_h = numeric(length(differences)),
  n_per_group = numeric(length(differences))
)

for (i in 1:length(differences)) {
  h_temp <- ES.h(prop_table$p1[i], prop_table$p2[i])
  result <- pwr.2p.test(h = h_temp, sig.level = 0.05, power = 0.80)
  prop_table$Cohen_h[i] <- round(h_temp, 3)
  prop_table$n_per_group[i] <- ceiling(result$n)
}

print(prop_table)

# ---------------------------------------------------------------------------
# Visualizing Effect of Baseline Proportion
# ---------------------------------------------------------------------------

# How does baseline proportion affect sample size?
baseline_props <- seq(0.1, 0.9, by = 0.1)
difference <- 0.15  # Fixed difference

sample_sizes_baseline <- sapply(baseline_props, function(p_base) {
  p_compare <- p_base + difference
  if (p_compare > 1) return(NA)  # Skip invalid proportions
  
  h_temp <- ES.h(p_compare, p_base)
  result <- pwr.2p.test(h = h_temp, sig.level = 0.05, power = 0.80)
  return(ceiling(result$n))
})

baseline_data <- data.frame(
  baseline_p = baseline_props,
  sample_size = sample_sizes_baseline
)

baseline_data <- baseline_data[!is.na(baseline_data$sample_size), ]

ggplot(baseline_data, aes(x = baseline_p, y = sample_size)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Sample Size vs Baseline Proportion",
       subtitle = "Fixed difference = 15%, α = 0.05, power = 0.80",
       x = "Baseline Proportion",
       y = "Sample Size per Group") +
  theme_minimal()

# ---------------------------------------------------------------------------
# Comparing Different Scenarios
# ---------------------------------------------------------------------------

cat("\n=== COMPARING PROPORTION TEST SCENARIOS ===\n\n")

scenarios <- data.frame(
  Scenario = c("High baseline", "Mid baseline", "Low baseline"),
  p1 = c(0.80, 0.50, 0.20),
  p2 = c(0.65, 0.35, 0.05),
  stringsAsFactors = FALSE
)

scenarios$Difference <- scenarios$p1 - scenarios$p2
scenarios$Cohen_h <- numeric(nrow(scenarios))
scenarios$n_per_group <- numeric(nrow(scenarios))

for (i in 1:nrow(scenarios)) {
  h_temp <- ES.h(scenarios$p1[i], scenarios$p2[i])
  result <- pwr.2p.test(h = h_temp, sig.level = 0.05, power = 0.80)
  scenarios$Cohen_h[i] <- round(h_temp, 3)
  scenarios$n_per_group[i] <- ceiling(result$n)
}

print(scenarios)
