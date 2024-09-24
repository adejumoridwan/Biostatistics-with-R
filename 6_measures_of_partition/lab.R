#install.packages("NHANES")
# Load the NHANES package and dataset
library(NHANES)

# Extract the BMI column 
bmi_data <- NHANES$BMI

# Remove NA values from the BMI column to ensure clean data
bmi_data <- na.omit(bmi_data)

# 1. Calculate Quartiles (25th, 50th, and 75th percentiles)
quartiles <- quantile(bmi_data, probs = c(0.25, 0.5, 0.75))
print(quartiles)

# 2. Calculate Deciles (10th, 20th, ..., 90th percentiles)
deciles <- quantile(bmi_data, probs = seq(0.1, 0.9, by = 0.1))
print(deciles)

# 3. Calculate Percentiles (1st to 99th percentiles)
percentiles <- quantile(bmi_data, probs = seq(0.01, 0.99, by = 0.01))
print(percentiles)

