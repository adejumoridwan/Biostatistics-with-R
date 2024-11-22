

# Binomial Distribution Example: Drug Success Rate

# A doctor is testing a new drug that has a 90% success rate. 
# He treats 20 patients and wants to know the probability that exactly 18 patients will be cured.


# Parameters
n <- 20  # number of trials (patients)
p <- 0.9 # probability of success (drug success rate)

# Calculate the probability of exactly 18 successes
dbinom(18, size = n, prob = p)

# Suppose the probability of a positive result for a genetic disorder is 0.1, and 15 individuals are tested. 
# What is the probability that exactly 3 individuals test positive?


# Parameters
n <- 15  # number of tests
p <- 0.1 # probability of a positive result

# Calculate the probability of exactly 3 positive tests
dbinom(3, size = n, prob = p)

# Poisson Distribution Example: Heart Attack Incidents

# On average, a hospital experiences 4 heart attacks per day. 
# What is the probability of having exactly 6 heart attacks in a given day?


# Parameters
lambda <- 4  # average number of occurrences (heart attacks per day)

# Calculate the probability of exactly 6 occurrences
dpois(6, lambda = lambda)


# On average, 2 cases of hospital-acquired infections are recorded per week. 
# What is the probability of observing 5 such cases in a particular week?

# Parameters
lambda <- 2  # average number of infections per week

# Calculate the probability of exactly 5 infections in a week
dpois(5, lambda = lambda)

# Normal Distribution Example: Blood Pressure Measurements

# Suppose systolic blood pressure in a population is normally distributed with a mean of 120 mmHg and a standard deviation of 15 mmHg. 
# What is the probability that a randomly selected individual will have a blood pressure between 110 mmHg and 130 mmHg?


# Parameters
mean_bp <- 120  # mean systolic blood pressure
sd_bp <- 15     # standard deviation of blood pressure

# Calculate the probability that blood pressure lies between 110 and 130
pnorm(130, mean = mean_bp, sd = sd_bp) - pnorm(110, mean = mean_bp, sd = sd_bp)


# Standardization
library(NHANES)

# Select relevant columns: ID, Cholesterol (TotChol), and Age for context
cholesterol_data <- NHANES[, c("ID", "TotChol", "Age")]

# Remove missing values (if any)
cholesterol_data <- na.omit(cholesterol_data)

# Calculate the mean and standard deviation of cholesterol levels
mean_cholesterol <- mean(cholesterol_data$TotChol, na.rm = TRUE)
sd_cholesterol <- sd(cholesterol_data$TotChol, na.rm = TRUE)

# Standardize the cholesterol levels (calculate z-scores)
cholesterol_data$Z_Score <- (cholesterol_data$TotChol - mean_cholesterol) / sd_cholesterol

# View the first few rows of the standardized data
head(cholesterol_data)

# Alternatively

head(scale(cholesterol_data$TotChol))

# Birth weights of babies in a certain population are generally distributed 
# with a mean of 3.2 kg and a standard deviation of 0.6 kg. 
# What is the probability that a randomly selected baby weighs between 
# 2.5 kg and 4.0 kg?

# Given parameters
mean_weight <- 3.2  # mean weight in kg
sd_weight <- 0.6    # standard deviation in kg

# Calculate the cumulative probabilities
P_4 <- pnorm(4.0, mean = mean_weight, sd = sd_weight)
P_2_5 <- pnorm(2.5, mean = mean_weight, sd = sd_weight)

# Probability that the weight is between 2.5 and 4.0
probability <- P_4 - P_2_5

# Print the result
probability

