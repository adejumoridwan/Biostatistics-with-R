# Create a sample dataset of blood glucose levels (mg/dL)
glucose_levels <- c(92, 98, 105, 110, 120, 125, 130, 135, 140, 150)

# Calculate range
glucose_range <- max(glucose_levels) - min(glucose_levels)

# Calculate variance
glucose_variance <- var(glucose_levels)

# Calculate standard deviation
glucose_sd <- sd(glucose_levels)

# Calculate mean (needed for coefficient of variation)
glucose_mean <- mean(glucose_levels)

# Calculate coefficient of variation
glucose_cv <- (glucose_sd / glucose_mean) * 100

# Print results
cat("Range:", glucose_range, "mg/dL\n")
cat("Variance:", glucose_variance, "(mg/dL)²\n")
cat("Standard Deviation:", glucose_sd, "mg/dL\n")
cat("Coefficient of Variation:", glucose_cv, "%\n")

# Optional: Create a summary table
summary_table <- data.frame(
  Statistic = c("Range", "Variance", "Standard Deviation", "Coefficient of Variation"),
  Value = c(glucose_range, glucose_variance, glucose_sd, glucose_cv)
)

print(summary_table)