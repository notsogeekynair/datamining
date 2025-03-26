# Feature Selection Method 1 (Information Gain)
# Install and load necessary package
install.packages("FSelectorRcpp")
library(FSelectorRcpp)

# For oversampled
# Compute Information Gain for each feature
info_gain_oversampled <- information_gain(Class ~ ., train_data_oversampled)

# Sort features by highest Information Gain
sorted_info_gain_oversampled <- info_gain_oversampled[order(-info_gain_oversampled$importance), ]
sorted_info_gain_oversampled

ggplot(sorted_info_gain_oversampled, aes(x = seq_along(importance), y = importance)) +
  geom_point() +
  geom_line() +
  labs(title = "Information Gain Scree Plot", x = "Feature Rank", y = "Information Gain") +
  theme_minimal()

N <- 15  # Keep top 15 features based on the elbow
selected_features_info_gain_oversampled <- sorted_info_gain_oversampled[1:N, "attributes"]
selected_features_info_gain_oversampled


# For undersampled
# Compute Information Gain for each feature
info_gain_undersampled <- information_gain(Class ~ ., train_data_undersampled)

# Sort features by highest Information Gain
sorted_info_gain_undersampled <- info_gain_undersampled[order(-info_gain_undersampled$importance), ]
sorted_info_gain_undersampled

ggplot(sorted_info_gain_undersampled, aes(x = seq_along(importance), y = importance)) +
  geom_point() +
  geom_line() +
  labs(title = "Information Gain Scree Plot", x = "Feature Rank", y = "Information Gain") +
  theme_minimal()

N <- 15# Keep top 15 features based on the elbow
selected_features_info_gain_undersampled <- sorted_info_gain_undersampled[1:N, "attributes"]
selected_features_info_gain_undersampled

# Feature Selection Method 2 (Boruta)
install.packages("Boruta")
library(Boruta)

# For oversampled
# Run Boruta for feature selection
set.seed(123)# Ensure reproducibility
boruta_result_oversampled <- Boruta(Class ~ ., data = train_data_oversampled, doTrace = 2)
boruta_result_oversampled
# Plot Boruta feature selection results
plot(boruta_result_oversampled, las = 2, cex.axis = 0.7)

# Get final feature importance decision
selected_features_boruta_oversampled <- getSelectedAttributes(boruta_result_oversampled, withTentative = FALSE)
selected_features_boruta_oversampled

# For undersampled
# Run Boruta for feature selection
set.seed(123)# Ensure reproducibility
boruta_result_undersampled <- Boruta(Class ~ ., data = train_data_undersampled, doTrace = 2)
boruta_result_undersampled
# Plot Boruta feature selection results
plot(boruta_result_undersampled, las = 2, cex.axis = 0.7)
# Resolve tentative attributes
boruta_result_fixed_undersampled <- TentativeRoughFix(boruta_result_undersampled)
# Extract confirmed features after resolving tentatives
selected_features_boruta_undersampled <- getSelectedAttributes(boruta_result_fixed_undersampled)
selected_features_boruta_undersampled

# Feature Selection Method 3 (Lasso Regularization)
# Load necessary packages
install.packages("glmnet")
library(glmnet)

# Function to apply LASSO on mixed numeric and factor data
apply_lasso_feature_selection <- function(data) {
  
  # Ensure Class is a factor
  data$Class <- as.factor(data$Class)
  
  # Convert factor variables to numeric (direct mapping)
  factor_columns <- sapply(data, is.factor)# Identify factor columns
  data[factor_columns] <- lapply(data[factor_columns], as.numeric)# Convert factors to numbers
  
  # Separate features (X) and target (y)
  X <- as.matrix(data[, -which(names(data) == "Class")])# Exclude target
  y <- as.numeric(data$Class) - 1# Convert target to binary (0/1)
  
  # Fit cross-validated Lasso logistic regression
  set.seed(123)# For reproducibility
  cv_model <- cv.glmnet(
    X, 
    y, 
    family = "binomial",# For binary classification
    alpha = 1,# Lasso penalty (L1 regularization)
    type.measure = "class",# Classification error as metric
    standardize = TRUE# Standardize features
  )
  
  # Extract non-zero coefficients at lambda.min (most predictive features)
  coefs <- coef(cv_model, s = "lambda.min")
  selected_features <- rownames(coefs)[which(coefs != 0)][-1]# Exclude intercept
  
  return(list(
    selected_features = selected_features,
    cv_model = cv_model
  ))
}

# Apply function to oversampled data
lasso_oversampled <- apply_lasso_feature_selection(train_data_oversampled)
selected_features_lasso_oversampled <- lasso_oversampled$selected_features
selected_features_lasso_oversampled

# Apply function to undersampled data
lasso_undersampled <- apply_lasso_feature_selection(train_data_undersampled)
selected_features_lasso_undersampled <- lasso_undersampled$selected_features
selected_features_lasso_undersampled