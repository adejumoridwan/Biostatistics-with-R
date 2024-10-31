# Load required libraries
library(stats)
library(ggplot2)

# Load the ToothGrowth dataset
data("ToothGrowth")
head(ToothGrowth)

# Example 1: One-Sample t-test
# Research Question: Is the average tooth length different from 18 mm?
# H0: μ = 18 (The mean tooth length is equal to 18 mm)
# H1: μ ≠ 18 (The mean tooth length is not equal to 18 mm)

# Perform one-sample t-test
one_sample_test <- t.test(ToothGrowth$len, mu = 18)
cat("\nOne-Sample t-test Results:")
print(one_sample_test)

