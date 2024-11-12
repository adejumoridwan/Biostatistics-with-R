
  
  # Load Libraries
library(MASS)
library(corrplot)
library(tidyverse)

# Load and Inspect Dataset
data("birthwt", package = "MASS")
birthwt <- birthwt %>%
  mutate(race = as.factor(race), 
         smoke = as.factor(smoke), 
         ht = as.factor(ht), 
         ui = as.factor(ui))

# View the first few rows
head(birthwt)

### Correlation Analysis

# Pearson Correlation between 'bwt' (birth weight) and 'lwt' (mother's weight)
correlation_pearson <- cor(birthwt$bwt, birthwt$lwt, method = "pearson")
cat("Pearson Correlation between birth weight and mother's weight:", correlation_pearson, "\n")

# Spearman Rank Correlation between 'bwt' and 'age' (mother's age)
correlation_spearman <- cor(birthwt$bwt, birthwt$age, method = "spearman")
cat("Spearman Correlation between birth weight and mother's age:", correlation_spearman, "\n")


# Visualizing cirrekatuib
# Plot a scatterplot between bwt (birth weight) and lwt (mother's weight)
plot(birthwt$lwt, birthwt$bwt,
     xlab = "Mother's Weight (lwt)",
     ylab = "Birth Weight (bwt)",
     main = "Scatterplot of Mother's Weight vs. Birth Weight",
     pch = 19,      # Set point shape
     col = "blue")  # Set point color

### Simple Linear Regression

# Simple Linear Regression: bwt ~ lwt
simple_lm <- lm(bwt ~ lwt, data = birthwt)
summary(simple_lm)

# Plot the regression line
plot(birthwt$lwt, birthwt$bwt, main = "Simple Linear Regression: Birth Weight vs Mother's Weight", 
     xlab = "Mother's Weight", ylab = "Birth Weight")
abline(simple_lm, col = "blue")

### Assumptions of Multiple Linear Regression

# Linearity Check: Residuals vs Fitted Values Plot
plot(multiple_lm$fitted.values, residuals(multiple_lm), 
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Normality Check: QQ Plot of Residuals
qqnorm(residuals(multiple_lm))
qqline(residuals(multiple_lm), col = "blue")

### Multiple Linear Regression

# Multiple Linear Regression: bwt ~ lwt + age + smoke
multiple_lm <- lm(bwt ~ lwt + age + smoke, data = birthwt)
summary(multiple_lm)



# Coefficient of Determination (R²)
cat("R-squared:", summary(multiple_lm)$r.squared, "\n")

### Logistic Regression (Binary Outcome Example)

# Create binary outcome for low birth weight (1 if < 2500 grams, otherwise 0)
birthwt$low_bwt <- ifelse(birthwt$bwt < 2500, 1, 0)

# Logistic Regression: Predict low birth weight based on mother's weight and smoking status
logistic_model <- glm(low_bwt ~ lwt + smoke, data = birthwt, family = binomial)
summary(logistic_model)

# Interpret the coefficients as odds ratios
exp(coef(logistic_model))  # Odds Ratios

