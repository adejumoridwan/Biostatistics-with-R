# ==============================================================================
# Lecture 94: Lab - Building a Clinical Prediction Model
# Practical Biostatistics with R
# ==============================================================================

# Load required packages
library(MASS)      # For stepAIC function
library(car)       # For VIF calculation
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation
library(gridExtra) # For arranging plots

# If packages are not installed, run:
# install.packages(c("MASS", "car", "ggplot2", "dplyr", "gridExtra"))

# ==============================================================================
# DATASET: We'll use the 'birthwt' dataset from MASS package
# This is a classic epidemiological dataset about risk factors for low birth weight
# ==============================================================================

# Load the dataset
data(birthwt)

# View the first few rows
head(birthwt)

# View dataset structure
str(birthwt)

# Dataset description:
# low:   indicator of birth weight < 2.5 kg (0 = normal, 1 = low)
# age:   mother's age in years
# lwt:   mother's weight in pounds at last menstrual period
# race:  mother's race (1 = white, 2 = black, 3 = other)
# smoke: smoking status during pregnancy (0 = no, 1 = yes)
# ptl:   number of previous premature labors
# ht:    history of hypertension (0 = no, 1 = yes)
# ui:    presence of uterine irritability (0 = no, 1 = yes)
# ftv:   number of physician visits during first trimester
# bwt:   birth weight in grams (our outcome variable)

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Create a working dataset
birth_data <- birthwt

# Convert categorical variables to factors with meaningful labels
birth_data$race <- factor(birth_data$race, 
                          levels = c(1, 2, 3),
                          labels = c("White", "Black", "Other"))

birth_data$smoke <- factor(birth_data$smoke, 
                           levels = c(0, 1),
                           labels = c("Non-smoker", "Smoker"))

birth_data$ht <- factor(birth_data$ht, 
                        levels = c(0, 1),
                        labels = c("No", "Yes"))

birth_data$ui <- factor(birth_data$ui, 
                        levels = c(0, 1),
                        labels = c("No", "Yes"))

# Summary statistics
summary(birth_data)

# Check for missing values
colSums(is.na(birth_data))

# ==============================================================================
# MULTIVARIABLE LINEAR REGRESSION
# ==============================================================================


# Simple linear regression (for comparison)
model_simple <- lm(bwt ~ age, data = birth_data)
summary(model_simple)

# simple regression r squared
summary(model_simple)$r.squared


# Multivariable linear regression
# Research question: What factors predict birth weight?
model_full <- lm(bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv, 
                 data = birth_data)

summary(model_full)

# Confidence intervals for coefficients
confint(model_full)

# ==============================================================================
# CHECKING MODEL ASSUMPTIONS
# ==============================================================================


# Extract residuals and fitted values
residuals <- residuals(model_full)
fitted_values <- fitted(model_full)
standardized_residuals <- rstandard(model_full)

# -----------------
# 1. LINEARITY CHECK
# -----------------

# Residuals vs Fitted Values plot
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)
lines(lowess(fitted_values, residuals), col = "green", lwd = 2)

##"Look for: Random scatter around zero (red line)
##"Warning signs: Curved patterns, systematic trends

# Partial residual plots for continuous predictors
# Age

plot(birth_data$age, residuals,
     main = "Residuals vs Age",
     xlab = "Mother's Age (years)",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Mother's weight
plot(birth_data$lwt, residuals,
     main = "Residuals vs Mother's Weight",
     xlab = "Mother's Weight (lbs)",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

# -----------------
# 2. INDEPENDENCE CHECK
# -----------------

# Check based on study design
# Are observations independent?
# No clustering (e.g., multiple births from same mother)?
# No time series structure?

# Plot residuals in order of data collection
plot(1:length(residuals), residuals,
     main = "Residuals in Order of Data Collection",
     xlab = "Observation Order",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)

##Look for: Random pattern (no trends or cycles)

# -----------------
# 3. NORMALITY CHECK
# -----------------

par(mfrow = c(1, 2))

# Q-Q plot
qqnorm(residuals, main = "Q-Q Plot of Residuals",
       pch = 19, col = rgb(0, 0, 1, 0.5))
qqline(residuals, col = "red", lwd = 2)

# Histogram
hist(residuals, breaks = 20, col = "lightblue", border = "white",
     main = "Histogram of Residuals",
     xlab = "Residuals",
     probability = TRUE)
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)),
      add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)
shapiro_test

# Conclusion: Residuals appear to be normally distributed (if p > 0.05)
# Conclusion: Evidence of non-normality (p < 0.05)
# Note: With large samples, minor deviations may be significant

# -----------------
# 4. HOMOSCEDASTICITY CHECK
# -----------------

par(mfrow = c(1, 2))

# Residuals vs Fitted
plot(fitted_values, residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Scale-Location plot
plot(fitted_values, sqrt(abs(standardized_residuals)),
     main = "Scale-Location Plot",
     xlab = "Fitted Values",
     ylab = "√|Standardized Residuals|",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = mean(sqrt(abs(standardized_residuals))), col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

# Look for: Constant vertical spread (no funnel or fan shape)

# Breusch-Pagan test for heteroscedasticity
bp_test <- lmtest::bptest(model_full)
bp_test
# Conclusion: No evidence of heteroscedasticity (p > 0.05)
# Conclusion: Evidence of heteroscedasticity (p < 0.05)

# Comprehensive diagnostic plots
par(mfrow = c(2, 2))
plot(model_full)
par(mfrow = c(1, 1))

# ==============================================================================
# LECTURE 91: MULTICOLLINEARITY & VIF
# ==============================================================================

# Calculate correlation matrix for continuous predictors
continuous_vars <- birth_data[, c("age", "lwt", "ptl", "ftv")]
cor_matrix <- cor(continuous_vars, use = "complete.obs")

print(round(cor_matrix, 3))

# Visualize correlation matrix
library(corrplot)
corrplot::corrplot(cor_matrix, method = "number", type = "upper",
                   title = "Correlation Matrix",
                   mar = c(0, 0, 2, 0))

# Calculate Variance Inflation Factor (VIF)
vif_values <- vif(model_full)
print(vif_values)

# VIF Interpretation
# VIF < 5: No multicollinearity concern
# 5 ≤ VIF < 10: Moderate multicollinearity
# "VIF ≥ 10: Severe multicollinearity



# Visualize VIF values
barplot(vif_values, 
        main = "Variance Inflation Factors",
        ylab = "VIF",
        col = ifelse(vif_values >= 10, "red",
                     ifelse(vif_values >= 5, "orange", "lightblue")),
        las = 2)
abline(h = 5, col = "orange", lty = 2, lwd = 2)
abline(h = 10, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("VIF < 5", "5 ≤ VIF < 10", "VIF ≥ 10"),
       fill = c("lightblue", "orange", "red"))

# ==============================================================================
# INTERACTION TERMS & EFFECT MODIFICATION
# ==============================================================================


# Research question: Does the effect of smoking on birth weight differ by race?

# Model without interaction
model_no_interaction <- lm(bwt ~ smoke + race, data = birth_data)

# Model with interaction
model_interaction <- lm(bwt ~ smoke * race, data = birth_data)

summary(model_no_interaction)

summary(model_interaction)

# Test if interaction is significant
# Testing for Interaction
anova_result <- anova(model_no_interaction, model_interaction)
print(anova_result)

# Conclusion: Significant interaction detected (p < 0.05) if the effect of smoking on birth weight differs across racial groups.
# Conclusion: No significant interaction (p > 0.05) if the effect of smoking is similar across racial groups.

# Visualize the interaction

# Calculate predicted values
pred_data <- expand.grid(
  smoke = levels(birth_data$smoke),
  race = levels(birth_data$race)
)

pred_data$predicted_bwt <- predict(model_interaction, newdata = pred_data)

# Create interaction plot
ggplot(pred_data, aes(x = race, y = predicted_bwt, color = smoke, group = smoke)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.2) +
  labs(title = "Interaction: Smoking × Race on Birth Weight",
       x = "Mother's Race",
       y = "Predicted Birth Weight (grams)",
       color = "Smoking Status") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Alternative visualization with actual data
ggplot(birth_data, aes(x = race, y = bwt, fill = smoke)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Birth Weight by Race and Smoking Status",
       x = "Mother's Race",
       y = "Birth Weight (grams)",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Example 2: Continuous × Continuous Interaction
# Does the effect of mother's age on birth weight depend on mother's weight?

model_continuous_interaction <- lm(bwt ~ age * lwt, data = birth_data)

# Continuous × Continuous Interaction: Age × Weight
summary(model_continuous_interaction)

# Visualize with predicted values at different levels
pred_data_cont <- expand.grid(
  age = seq(min(birth_data$age), max(birth_data$age), length.out = 50),
  lwt = quantile(birth_data$lwt, probs = c(0.25, 0.5, 0.75))
)

pred_data_cont$predicted_bwt <- predict(model_continuous_interaction, 
                                        newdata = pred_data_cont)
pred_data_cont$lwt_cat <- factor(pred_data_cont$lwt,
                                 labels = c("Low Weight (25th %ile)",
                                            "Medium Weight (50th %ile)",
                                            "High Weight (75th %ile)"))

ggplot(pred_data_cont, aes(x = age, y = predicted_bwt, color = lwt_cat)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Interaction: Mother's Age × Weight on Birth Weight",
       x = "Mother's Age (years)",
       y = "Predicted Birth Weight (grams)",
       color = "Mother's Weight") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# ==============================================================================
# MODEL SELECTION (AIC, BIC, Stepwise)
# ==============================================================================

# Compare models using AIC and BIC

# Define several candidate models
model1 <- lm(bwt ~ age, data = birth_data)
model2 <- lm(bwt ~ age + lwt, data = birth_data)
model3 <- lm(bwt ~ age + lwt + smoke, data = birth_data)
model4 <- lm(bwt ~ age + lwt + smoke + race, data = birth_data)
model5 <- model_full  # All predictors

# Create comparison table
model_comparison <- data.frame(
  Model = c("Age only", 
            "Age + Weight",
            "Age + Weight + Smoke",
            "Age + Weight + Smoke + Race",
            "Full model (all predictors)"),
  Predictors = c(1, 2, 3, 5, 8),
  R_squared = c(summary(model1)$r.squared,
                summary(model2)$r.squared,
                summary(model3)$r.squared,
                summary(model4)$r.squared,
                summary(model5)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared,
                    summary(model3)$adj.r.squared,
                    summary(model4)$adj.r.squared,
                    summary(model5)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model5)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5))
)

print(model_comparison)


# Calculate delta AIC and BIC
model_comparison$Delta_AIC <- model_comparison$AIC - min(model_comparison$AIC)
model_comparison$Delta_BIC <- model_comparison$BIC - min(model_comparison$BIC)

print(model_comparison)

# -----------------
# STEPWISE SELECTION
# -----------------


# Forward selection
model_null <- lm(bwt ~ 1, data = birth_data)  # Intercept only
model_forward <- stepAIC(model_null, 
                         scope = list(lower = model_null, upper = model_full),
                         direction = "forward",
                         trace = TRUE)

summary(model_forward)

# Backward elimination
model_backward <- stepAIC(model_full, 
                          direction = "backward",
                          trace = TRUE)

summary(model_backward)

# Stepwise (both directions)
model_stepwise <- stepAIC(model_null,
                          scope = list(lower = model_null, upper = model_full),
                          direction = "both",
                          trace = TRUE)

summary(model_stepwise)

# Compare stepwise results
AIC(model_forward)
AIC(model_backward)
AIC(model_stepwise)


# -----------------
# MODEL VALIDATION
# -----------------

# Split data into training and test sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(birth_data), size = 0.7 * nrow(birth_data))
train_data <- birth_data[train_index, ]
test_data <- birth_data[-train_index, ]

nrow(train_data)
nrow(test_data)

# Fit model on training data
model_train <- lm(bwt ~ lwt + race + smoke + ht + ui, data = train_data)

# Predictions on test data
predictions <- predict(model_train, newdata = test_data)

# Calculate performance metrics
actual <- test_data$bwt
residuals_test <- actual - predictions

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals_test^2))

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals_test))

# R-squared on test data
ss_res <- sum(residuals_test^2)
ss_tot <- sum((actual - mean(actual))^2)
r_squared_test <- 1 - (ss_res / ss_tot)

cat("--- Test Set Performance ---\n")
cat("RMSE:", round(rmse, 2), "grams\n")
cat("MAE:", round(mae, 2), "grams\n")
cat("R-squared (test):", round(r_squared_test, 3), "\n")
cat("R-squared (training):", round(summary(model_train)$r.squared, 3), "\n")

# Plot predicted vs actual
plot(actual, predictions,
     main = "Predicted vs Actual Birth Weight (Test Set)",
     xlab = "Actual Birth Weight (grams)",
     ylab = "Predicted Birth Weight (grams)",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = "Perfect prediction", col = "red", lty = 2, lwd = 2)

# ==============================================================================
# FINAL MODEL AND INTERPRETATION
# ==============================================================================


# Based on our analyses, select final model
# Let's use the backward elimination result as our final model
final_model <- model_backward

summary(final_model)

cat("\n--- Model Diagnostics ---\n")
cat("R-squared:", round(summary(final_model)$r.squared, 3), "\n")
cat("Adjusted R-squared:", round(summary(final_model)$adj.r.squared, 3), "\n")
cat("AIC:", round(AIC(final_model), 2), "\n")
cat("BIC:", round(BIC(final_model), 2), "\n")

# Check assumptions for final model
cat("\n--- Checking Assumptions for Final Model ---\n")
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

# VIF for final model
cat("\n--- VIF for Final Model ---\n")
print(vif(final_model))


