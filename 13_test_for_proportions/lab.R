# Load necessary libraries
if(!require(NHANES)) install.packages("NHANES")
library(NHANES)

# Clean and inspect the data
data(NHANES)
str(NHANES)

# Filter data to include only relevant columns
nhanes_data <- NHANES[, c("Gender", "BPSysAve", "SmokeNow")]

# Create a binary variable for high systolic blood pressure (BPSysAve > 120)
nhanes_data$HighBPSysAve <- ifelse(nhanes_data$BPSysAve > 120, "Yes", "No")

# Filter out missing values for BPSysAve and SmokeNow
nhanes_data <- na.omit(nhanes_data)

### Test of Proportion

# Let's test the proportion of males with high systolic blood pressure.
# For simplicity, we will consider only male participants.
male_data <- subset(nhanes_data, Gender == "male")
n_male <- nrow(male_data)
n_high_bp_male <- sum(male_data$HighBPSysAve == "Yes")

# Proportion of males with high systolic blood pressure
p_hat <- n_high_bp_male / n_male
p_hat

# One-sample z-test for proportion
# Null hypothesis: The proportion of males with high BP is 0.5
prop.test(x = n_high_bp_male, n = n_male, p = 0.5, correct = FALSE)



# Create a binary variable for high systolic blood pressure (BPSysAve > 120)
nhanes_data$HighBPSysAve <- ifelse(nhanes_data$BPSysAve > 120, "Yes", "No")

# Filter out missing values for BPSysAve and Gender
nhanes_data <- na.omit(nhanes_data)

# Calculate counts for males and females with high blood pressure
# Separate data by gender
male_data <- subset(nhanes_data, Gender == "male")
female_data <- subset(nhanes_data, Gender == "female")

# Count number of successes (High BP) and sample size for each gender
n_male <- nrow(male_data)
n_female <- nrow(female_data)
n_high_bp_male <- sum(male_data$HighBPSysAve == "Yes")
n_high_bp_female <- sum(female_data$HighBPSysAve == "Yes")

# Two-sample Z-test for population proportions
# Null hypothesis: The proportions of high BP in males and females are the same.
prop.test(
  x = c(n_high_bp_male, n_high_bp_female),  # number of successes (high BP)
  n = c(n_male, n_female),                  # sample sizes
  correct = FALSE                           # without Yates' continuity correction for large samples
)

# Results summary
list(
  male_count = n_male,
  female_count = n_female,
  male_high_bp = n_high_bp_male,
  female_high_bp = n_high_bp_female,
  two_sample_z_test = prop.test(x = c(n_high_bp_male, n_high_bp_female), n = c(n_male, n_female), correct = FALSE)
)


### Test of Association

# Chi-Square Test of Independence between Smoking and High Blood Pressure
# Create a contingency table for Smoking and High Systolic BP
table_smoke_bp <- table(nhanes_data$SmokeNow, nhanes_data$HighBPSysAve)

# View the contingency table
table_smoke_bp

# Conduct Chi-Square Test of Independence
chisq_test <- chisq.test(table_smoke_bp)
chisq_test

# Fisher's Exact Test (if the Chi-Square test assumptions are violated)
fisher_test <- fisher.test(table_smoke_bp)
fisher_test

# Results summary
list(
  proportion_test = prop.test(x = n_high_bp_male, n = n_male, p = 0.5, correct = FALSE),
  chi_square_test = chisq_test,
  fisher_test = fisher_test
)
