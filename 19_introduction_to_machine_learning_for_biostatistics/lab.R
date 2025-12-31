
# Load required packages
# Install if needed: install.packages(c("tidyverse", "caret", "class", "rpart", 
#                                       "rpart.plot", "randomForest", "pROC"))

library(tidyverse)
library(caret)        # For machine learning workflows
library(class)        # For KNN
library(rpart)        # For decision trees
library(rpart.plot)   # For visualizing trees
library(randomForest) # For random forest
library(pROC)         # For ROC curves

# Set seed for reproducibility
set.seed(123)

# =============================================================================
# 1. LOAD AND EXPLORE THE DATA
# =============================================================================

# For this lab, we'll create a simulated realistic dataset

# Create simulated heart disease data
n <- 500

heart_data <- tibble(
  age = round(rnorm(n, mean = 54, sd = 9)),
  sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.68, 0.32)),
  chest_pain_type = sample(c("Typical", "Atypical", "Non-anginal", "Asymptomatic"), 
                           n, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25)),
  resting_bp = round(rnorm(n, mean = 131, sd = 17)),
  cholesterol = round(rnorm(n, mean = 246, sd = 51)),
  fasting_bs = sample(c("Normal", "High"), n, replace = TRUE, prob = c(0.85, 0.15)),
  max_heart_rate = round(rnorm(n, mean = 149, sd = 22)),
  exercise_angina = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.67, 0.33)),
  st_depression = round(abs(rnorm(n, mean = 1.04, sd = 1.16)), 1),
  num_major_vessels = sample(0:3, n, replace = TRUE, prob = c(0.45, 0.25, 0.20, 0.10))
) %>%
  mutate(
    # Create outcome based on realistic risk factors
    risk_score = 0.05 * age + 
      ifelse(sex == "Male", 15, 0) +
      ifelse(chest_pain_type == "Asymptomatic", 20, 
             ifelse(chest_pain_type == "Typical", 15, 5)) +
      0.02 * resting_bp +
      0.01 * cholesterol +
      ifelse(fasting_bs == "High", 10, 0) +
      -0.05 * max_heart_rate +
      ifelse(exercise_angina == "Yes", 15, 0) +
      5 * st_depression +
      8 * num_major_vessels,
    prob_disease = plogis((risk_score - 50) / 10),
    heart_disease = factor(ifelse(runif(n) < prob_disease, "Yes", "No"),
                           levels = c("No", "Yes"))
  ) %>%
  select(-risk_score, -prob_disease)

# View the data
glimpse(heart_data)
head(heart_data)

# Summary statistics
summary(heart_data)

# Check for missing values
sum(is.na(heart_data))

# Visualize outcome distribution
table(heart_data$heart_disease)
prop.table(table(heart_data$heart_disease))

ggplot(heart_data, aes(x = heart_disease, fill = heart_disease)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of Heart Disease",
       x = "Heart Disease Status",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"))

# =============================================================================
# 2. EXPLORATORY DATA ANALYSIS
# =============================================================================

# Age distribution by disease status
ggplot(heart_data, aes(x = heart_disease, y = age, fill = heart_disease)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Heart Disease Status",
       x = "Heart Disease",
       y = "Age (years)") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"))

# Cholesterol by disease status
ggplot(heart_data, aes(x = heart_disease, y = cholesterol, fill = heart_disease)) +
  geom_boxplot() +
  labs(title = "Cholesterol by Heart Disease Status",
       x = "Heart Disease",
       y = "Cholesterol (mg/dl)") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"))

# Sex distribution
heart_data %>%
  count(sex, heart_disease) %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = sex, y = prop, fill = heart_disease)) +
  geom_col(position = "fill") +
  labs(title = "Heart Disease Prevalence by Sex",
       x = "Sex",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral"))

# Chest pain type
heart_data %>%
  count(chest_pain_type, heart_disease) %>%
  group_by(chest_pain_type) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = chest_pain_type, y = prop, fill = heart_disease)) +
  geom_col(position = "fill") +
  labs(title = "Heart Disease by Chest Pain Type",
       x = "Chest Pain Type",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral")) +
  coord_flip()

# =============================================================================
# 3. DATA PREPROCESSING
# =============================================================================

# Create dummy variables for categorical predictors
heart_processed <- heart_data %>%
  mutate(
    sex_male = ifelse(sex == "Male", 1, 0),
    cp_typical = ifelse(chest_pain_type == "Typical", 1, 0),
    cp_atypical = ifelse(chest_pain_type == "Atypical", 1, 0),
    cp_nonanginal = ifelse(chest_pain_type == "Non-anginal", 1, 0),
    fasting_bs_high = ifelse(fasting_bs == "High", 1, 0),
    exercise_angina_yes = ifelse(exercise_angina == "Yes", 1, 0)
  )

# Select features for modeling
features <- c("age", "sex_male", "cp_typical", "cp_atypical", "cp_nonanginal",
              "resting_bp", "cholesterol", "fasting_bs_high", "max_heart_rate",
              "exercise_angina_yes", "st_depression", "num_major_vessels")

X <- heart_processed[, features]
y <- heart_processed$heart_disease

# =============================================================================
# 4. TRAIN-TEST SPLIT
# =============================================================================

# Create 70-30 train-test split
train_index <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Check dimensions
cat("Training set size:", nrow(X_train), "\n")
cat("Test set size:", nrow(X_test), "\n")
cat("Training set disease prevalence:", mean(y_train == "Yes"), "\n")
cat("Test set disease prevalence:", mean(y_test == "Yes"), "\n")

# =============================================================================
# 5. K-NEAREST NEIGHBORS (KNN)
# =============================================================================


# KNN requires feature scaling (standardization)
# We'll use the preProcess function from caret

# Calculate scaling parameters from training data only
preproc <- preProcess(X_train, method = c("center", "scale"))

# Apply scaling to both train and test sets
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)

# Try different values of K
k_values <- c(1, 3, 5, 7, 9, 11, 15, 21)
knn_results <- tibble()

for (k in k_values) {
  # Train KNN
  knn_pred <- knn(train = X_train_scaled,
                  test = X_test_scaled,
                  cl = y_train,
                  k = k,
                  prob = TRUE)
  
  # Calculate accuracy
  cm <- confusionMatrix(knn_pred, y_test, positive = "Yes")
  
  knn_results <- bind_rows(knn_results, 
                           tibble(k = k,
                                  accuracy = cm$overall["Accuracy"],
                                  sensitivity = cm$byClass["Sensitivity"],
                                  specificity = cm$byClass["Specificity"]))
}

# Display results
print(knn_results)

# Plot accuracy by K
ggplot(knn_results, aes(x = k, y = accuracy)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "KNN Performance by K Value",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal() +
  scale_x_continuous(breaks = k_values)

# Select best K
best_k <- knn_results$k[which.max(knn_results$accuracy)]
best_k

# Train final KNN model with best K
knn_pred_final <- knn(train = X_train_scaled,
                      test = X_test_scaled,
                      cl = y_train,
                      k = best_k,
                      prob = TRUE)

# Detailed performance evaluation
cm_knn <- confusionMatrix(knn_pred_final, y_test, positive = "Yes")
print(cm_knn)

# =============================================================================
# 6. DECISION TREE
# =============================================================================


# Combine features and outcome for rpart
train_data <- cbind(X_train, heart_disease = y_train)
test_data <- cbind(X_test, heart_disease = y_test)

# Train decision tree
# We'll use cross-validation to find optimal complexity parameter
tree_model <- rpart(heart_disease ~ .,
                    data = train_data,
                    method = "class",
                    control = rpart.control(cp = 0.01, minsplit = 20))

# View tree structure
print(tree_model)

# Visualize the tree
rpart.plot(tree_model,
           type = 4,
           extra = 101,
           under = TRUE,
           faclen = 0,
           main = "Decision Tree for Heart Disease Prediction")

# Variable importance
var_imp <- tree_model$variable.importance
var_imp_df <- tibble(
  Variable = names(var_imp),
  Importance = var_imp
) %>%
  arrange(desc(Importance))

print(var_imp_df)

# Plot variable importance
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance in Decision Tree",
       x = "Variable",
       y = "Importance") +
  theme_minimal()

# Make predictions
tree_pred <- predict(tree_model, test_data, type = "class")
tree_pred_prob <- predict(tree_model, test_data, type = "prob")[, 2]

# Confusion matrix
cm_tree <- confusionMatrix(tree_pred, y_test, positive = "Yes")
print(cm_tree)

# Pruning the tree
# Check complexity parameter table
printcp(tree_model)
plotcp(tree_model)

# Prune to optimal cp (lowest xerror)
best_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
tree_pruned <- prune(tree_model, cp = best_cp)

# Visualize pruned tree
rpart.plot(tree_pruned,
           type = 4,
           extra = 101,
           under = TRUE,
           faclen = 0,
           main = "Pruned Decision Tree")

# Predictions with pruned tree
tree_pruned_pred <- predict(tree_pruned, test_data, type = "class")
cm_tree_pruned <- confusionMatrix(tree_pruned_pred, y_test, positive = "Yes")
print(cm_tree_pruned)

# =============================================================================
# 7. RANDOM FOREST
# =============================================================================


# Train random forest
# ntree = number of trees, mtry = number of features per split
rf_model <- randomForest(heart_disease ~ .,
                         data = train_data,
                         ntree = 500,
                         mtry = sqrt(ncol(X_train)),
                         importance = TRUE)

print(rf_model)

# Plot error rate by number of trees
plot(rf_model, main = "Random Forest Error Rate by Number of Trees")
legend("topright", legend = colnames(rf_model$err.rate), 
       col = 1:3, lty = 1:3, cex = 0.8)

# Variable importance
importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance in Random Forest")

# Create importance dataframe
rf_importance <- as.data.frame(importance(rf_model))
rf_importance$Variable <- rownames(rf_importance)

# Plot Mean Decrease in Accuracy
ggplot(rf_importance, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                          y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       subtitle = "Mean Decrease in Accuracy",
       x = "Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()

# Make predictions
rf_pred <- predict(rf_model, test_data, type = "class")
rf_pred_prob <- predict(rf_model, test_data, type = "prob")[, 2]

# Confusion matrix
cm_rf <- confusionMatrix(rf_pred, y_test, positive = "Yes")
print(cm_rf)

# Tune mtry parameter using cross-validation
tune_rf <- tuneRF(X_train, y_train,
                  ntreeTry = 500,
                  stepFactor = 1.5,
                  improve = 0.01,
                  trace = TRUE,
                  plot = TRUE)

# Optimal mtry
best_mtry <- tune_rf[which.min(tune_rf[, 2]), 1]
cat("\nOptimal mtry:", best_mtry, "\n")

# Train final model with optimal mtry
rf_final <- randomForest(heart_disease ~ .,
                         data = train_data,
                         ntree = 500,
                         mtry = best_mtry,
                         importance = TRUE)

rf_final_pred <- predict(rf_final, test_data, type = "class")
rf_final_pred_prob <- predict(rf_final, test_data, type = "prob")[, 2]

cm_rf_final <- confusionMatrix(rf_final_pred, y_test, positive = "Yes")
print(cm_rf_final)

# =============================================================================
# 8. MODEL COMPARISON
# =============================================================================


# Extract performance metrics
models_comparison <- tibble(
  Model = c("KNN", "Decision Tree", "Pruned Tree", "Random Forest", "Tuned RF"),
  Accuracy = c(cm_knn$overall["Accuracy"],
               cm_tree$overall["Accuracy"],
               cm_tree_pruned$overall["Accuracy"],
               cm_rf$overall["Accuracy"],
               cm_rf_final$overall["Accuracy"]),
  Sensitivity = c(cm_knn$byClass["Sensitivity"],
                  cm_tree$byClass["Sensitivity"],
                  cm_tree_pruned$byClass["Sensitivity"],
                  cm_rf$byClass["Sensitivity"],
                  cm_rf_final$byClass["Sensitivity"]),
  Specificity = c(cm_knn$byClass["Specificity"],
                  cm_tree$byClass["Specificity"],
                  cm_tree_pruned$byClass["Specificity"],
                  cm_rf$byClass["Specificity"],
                  cm_rf_final$byClass["Specificity"]),
  Precision = c(cm_knn$byClass["Precision"],
                cm_tree$byClass["Precision"],
                cm_tree_pruned$byClass["Precision"],
                cm_rf$byClass["Precision"],
                cm_rf_final$byClass["Precision"])
)

print(models_comparison)

# Visualize comparison
models_comparison %>%
  pivot_longer(cols = c(Accuracy, Sensitivity, Specificity, Precision),
               names_to = "Metric",
               values_to = "Value") %>%
  ggplot(aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Performance Comparison",
       x = "Model",
       y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# =============================================================================
# 9. ROC CURVES
# =============================================================================


# Get predicted probabilities (need to reconstruct for KNN)
# For KNN, we'll use the prob attribute
knn_prob <- attr(knn_pred_final, "prob")
knn_prob_yes <- ifelse(knn_pred_final == "Yes", knn_prob, 1 - knn_prob)

# Create ROC objects
roc_knn <- roc(y_test, knn_prob_yes)
roc_tree <- roc(y_test, tree_pred_prob)
roc_rf <- roc(y_test, rf_final_pred_prob)

# Calculate AUC
auc_knn <- auc(roc_knn)
auc_tree <- auc(roc_tree)
auc_rf <- auc(roc_rf)

cat("KNN AUC:", round(auc_knn, 3), "\n")
cat("Decision Tree AUC:", round(auc_tree, 3), "\n")
cat("Random Forest AUC:", round(auc_rf, 3), "\n")

# Plot ROC curves
plot(roc_knn, col = "blue", main = "ROC Curves Comparison")
plot(roc_tree, col = "green", add = TRUE)
plot(roc_rf, col = "red", add = TRUE)
legend("bottomright", 
       legend = c(paste("KNN (AUC =", round(auc_knn, 3), ")"),
                  paste("Tree (AUC =", round(auc_tree, 3), ")"),
                  paste("RF (AUC =", round(auc_rf, 3), ")")),
       col = c("blue", "green", "red"),
       lwd = 2)

# =============================================================================
# 10. CROSS-VALIDATION FOR ROBUST EVALUATION
# =============================================================================


# Set up 10-fold cross-validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

# Prepare data (caret needs specific format)
cv_data <- heart_processed %>%
  select(all_of(features), heart_disease) %>%
  mutate(heart_disease = make.names(heart_disease))  # Make valid names

# KNN with cross-validation
cv_knn <- train(heart_disease ~ .,
                data = cv_data,
                method = "knn",
                trControl = train_control,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(k = seq(1, 21, 2)),
                metric = "ROC")

print(cv_knn)
plot(cv_knn)

# Decision tree with cross-validation
cv_tree <- train(heart_disease ~ .,
                 data = cv_data,
                 method = "rpart",
                 trControl = train_control,
                 tuneLength = 10,
                 metric = "ROC")

print(cv_tree)
plot(cv_tree)

# Random forest with cross-validation
cv_rf <- train(heart_disease ~ .,
               data = cv_data,
               method = "rf",
               trControl = train_control,
               tuneGrid = expand.grid(mtry = c(2, 3, 4, 5, 6)),
               ntree = 500,
               metric = "ROC")

print(cv_rf)
plot(cv_rf)

# Compare models
cv_results <- resamples(list(KNN = cv_knn,
                             Tree = cv_tree,
                             RF = cv_rf))
summary(cv_results)

# Visualize cross-validation results
bwplot(cv_results, metric = "ROC")
dotplot(cv_results, metric = "ROC")

# =============================================================================
# 11. PRACTICAL APPLICATION: PREDICTING NEW PATIENTS
# =============================================================================


# Create new patient profiles
new_patients <- tibble(
  patient_id = 1:3,
  age = c(45, 62, 58),
  sex = c("Male", "Female", "Male"),
  chest_pain_type = c("Atypical", "Asymptomatic", "Typical"),
  resting_bp = c(120, 140, 130),
  cholesterol = c(200, 280, 240),
  fasting_bs = c("Normal", "High", "Normal"),
  max_heart_rate = c(160, 135, 145),
  exercise_angina = c("No", "Yes", "No"),
  st_depression = c(0.5, 2.1, 1.0),
  num_major_vessels = c(0, 2, 1)
)

# Preprocess new patients
new_patients_processed <- new_patients %>%
  mutate(
    sex_male = ifelse(sex == "Male", 1, 0),
    cp_typical = ifelse(chest_pain_type == "Typical", 1, 0),
    cp_atypical = ifelse(chest_pain_type == "Atypical", 1, 0),
    cp_nonanginal = ifelse(chest_pain_type == "Non-anginal", 1, 0),
    fasting_bs_high = ifelse(fasting_bs == "High", 1, 0),
    exercise_angina_yes = ifelse(exercise_angina == "Yes", 1, 0)
  ) %>%
  select(patient_id, all_of(features))

# Make predictions with all models
new_X <- new_patients_processed %>% select(-patient_id)

# KNN predictions (need to scale first)
new_X_scaled <- predict(preproc, new_X)
knn_new_pred <- knn(train = X_train_scaled,
                    test = new_X_scaled,
                    cl = y_train,
                    k = best_k)

# Tree predictions
tree_new_pred <- predict(tree_pruned, new_X, type = "class")
tree_new_prob <- predict(tree_pruned, new_X, type = "prob")[, 2]

# Random Forest predictions
rf_new_pred <- predict(rf_final, new_X, type = "class")
rf_new_prob <- predict(rf_final, new_X, type = "prob")[, 2]

# Combine results
predictions_summary <- new_patients %>%
  mutate(
    KNN_Prediction = knn_new_pred,
    Tree_Prediction = tree_new_pred,
    Tree_Probability = round(tree_new_prob, 3),
    RF_Prediction = rf_new_pred,
    RF_Probability = round(rf_new_prob, 3)
  )

print(predictions_summary)

# =============================================================================
# Save the best model for future use
# =============================================================================

# Save the model
saveRDS(rf_final, "heart_disease_rf_model.rds")

# To load later:
loaded_model <- readRDS("heart_disease_rf_model.rds")
#predictions <- predict(loaded_model, new_data)