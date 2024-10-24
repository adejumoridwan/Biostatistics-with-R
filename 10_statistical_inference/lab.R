Here is the R code to illustrate the lessons from the document:

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Lesson 1: Statistical Inference
# --------------------------------
# Explanation: Making conclusions about a population based on information from a sample.

# Generate a population (normally distributed data) for illustration
set.seed(123)
population <- rnorm(10000, mean = 100, sd = 15)  # Population with mean 100 and SD 15

# Take a random sample from the population
sample_data <- sample(population, 100)

# Plotting the population and sample distribution
population_plot <- ggplot(data = data.frame(population), aes(x = population)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black", alpha = 0.7) +
  ggtitle("Population Distribution") + 
  xlab("Values") + ylab("Frequency") + theme_minimal()

sample_plot <- ggplot(data = data.frame(sample_data), aes(x = sample_data)) +
  geom_histogram(bins = 20, fill = "lightgreen", color = "black", alpha = 0.7) +
  ggtitle("Sample Distribution") + 
  xlab("Values") + ylab("Frequency") + theme_minimal()

# Display the plots
print(population_plot)
print(sample_plot)

# Lesson 2: Point Estimate
# --------------------------------
# Mean of the sample as a point estimate of population mean
sample_mean <- mean(sample_data)
population_mean <- mean(population)

cat("Sample Mean (Point Estimate): ", sample_mean, "\n")
cat("Population Mean: ", population_mean, "\n")

# Lesson 3: Sampling Distribution and Central Limit Theorem
# --------------------------------
# Simulating sampling distribution of the sample mean
sample_means <- replicate(1000, mean(sample(sample_data, 50)))

# Plot the sampling distribution
sampling_dist_plot <- ggplot(data = data.frame(sample_means), aes(x = sample_means)) +
  geom_histogram(bins = 30, fill = "coral", color = "black", alpha = 0.7) +
  ggtitle("Sampling Distribution of Sample Means") +
  xlab("Sample Means") + ylab("Frequency") + theme_minimal()

# Display the sampling distribution plot
print(sampling_dist_plot)

# Lesson 4: Standard Error
# --------------------------------
# Function to calculate Standard Error
SE <- function(x) {
  return(sd(x) / sqrt(length(x)))
}

sample_se <- SE(sample_data)
cat("Standard Error of the Sample Mean: ", sample_se, "\n")

# Lesson 5: Confidence Interval
# --------------------------------
# Calculate 95% confidence interval for the sample mean
z_score <- 1.96  # For 95% confidence
margin_of_error <- z_score * sample_se
conf_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)

cat("95% Confidence Interval for the Sample Mean: [", conf_interval[1], ", ", conf_interval[2], "]\n")

# Visualizing Confidence Interval
ci_plot <- ggplot(data = data.frame(sample_data), aes(x = sample_data)) +
  geom_histogram(bins = 20, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(xintercept = conf_interval, color = "red", linetype = "dashed", size = 1) +
  ggtitle("Sample Distribution with 95% Confidence Interval") +
  xlab("Values") + ylab("Frequency") + theme_minimal()

# Display the confidence interval plot
print(ci_plot)

Explanation:

1. Population and Sample Visualization: The population_plot shows the full population's distribution, while the sample_plot shows the distribution of a random sample from that population.


2. Point Estimate: The code calculates the mean of the sample as a point estimate for the population mean.


3. Sampling Distribution: This simulates the distribution of sample means from multiple random samples to demonstrate the Central Limit Theorem.


4. Standard Error: The function SE computes the standard error of the sample mean.


5. Confidence Interval: A 95% confidence interval for the sample mean is calculated and visualized.



You can insert relevant images for the "add image" parts in your slide presentation where required.

