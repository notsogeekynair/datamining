# IMPORT LIBRARIES
install.packages("caret")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("corrplot")
library(caret)
library(reshape2)
library(ggplot2)
library(corrplot)

# LOAD AND INSPECT DATA
# Load the dataset
df <- read.csv('project_data.csv')
# Check dimensions
dim(df)
head(df)

# Remove SERIALNO
df <- df[, !(names(df) %in% c("SERIALNO"))]

# Check for duplicate rows
cat("Number of duplicate rows:", nrow(df[duplicated(df), ]))

# HANDLING NA VALUES
# Check the number of missing values in each column
sapply(df, function(x) sum(is.na(x)))

# Remove the columns with more than 80% of missing values
drop_columns <- names(which(colMeans(is.na(df)) > 0.8))
drop_columns
df <- df[, !names(df) %in% drop_columns]
dim(df)

# Check the number of missing values in each column 
# after dropping columns with more than 80% of missing values
sapply(df, function(x) sum(is.na(x)))

# Mapping for NA imputation for every column containing NA values
impute_map <- list(
  COW = 0, POVPIP = -1, JWRIP = 0, ENG = 0, PERNP = 0, JWMNP = 0, 
  FER = 0, SCIENGRLP = 0, WKHP = 0, GCL = 0, SCIENGP = 0, MARHYP = -1,
  RC = -1, WKWN = 0, POWSP = 0, YOEP = -1, MARHD = 0, POWPUMA = 0, MARHM = 0, 
  PAOC = 0, MARHT = 0, OC = -1, MARHW = 0, MIL = 0, LANP = 0, JWDP = 0, 
  JWAP = 0, INDP = 0, FOD1P = 0, ESR = 0, NWAB = 0, NWAV = 0, DRIVESP = 0, 
  NWLA = 0, DECADE = 0, NWLK = 0, OCCP = 0, NWRE = 0, WRK = 0, JWTRNS = 0)

# Loop through each column and impute the specified value
for (col in names(impute_map)) {
  if (col %in% names(df)) {
    df[[col]][is.na(df[[col]])] <- impute_map[[col]]
  }
}

# Check the number of missing values in each column after imputation
sapply(df, function(x) sum(is.na(x)))

# DATA TYPES CORRECTION
# Check the current data type of each variable
sapply(df, class)

# Convert the columns in cols_to_factor to factors
cols_to_factor <- c("RT", "DIVISION", "PUMA", "REGION",
                    "STATE", "ADJINC", "CIT", "COW", "ENG",
                    "FER", "GCL", "HIMRKS", "HINS1", "HINS2", 
                    "HINS3", "HINS4", "HINS5", "HINS6", "HINS7", "JWTRNS", 
                    "LANX", "MAR", "MARHD", "MARHM", "MARHT", "MARHW", "MIG", 
                    "MIL", "NWAB", "NWAV", "NWLA", "NWLK", "NWRE", "SCH",
                    "SCHL", "SEX", "WRK", "ANC", "ANC1P", "ANC2P", "DECADE", 
                    "DRIVESP", "ESR", "FOD1P", "HICOV", "HISP", "INDP", "JWAP", 
                    "JWDP", "LANP", "MSP", "NATIVITY", "OC", "OCCP", "PAOC",
                    "POBP", "POWPUMA", "POWSP", "PRIVCOV", "PUBCOV", "QTRBIR",
                    "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACASN", "RACBLK",
                    "RACNH", "RACNUM", "RACPI", "RACSOR", "RACWHT", "RC", "SCIENGP",
                    "SCIENGRLP", "WAOB", "Class")

df[cols_to_factor] <- lapply(df[cols_to_factor], as.factor)

# Convert integer variables in int_to_numeric to numeric
int_to_numeric <- sapply(df, is.integer)
df[int_to_numeric] <- lapply(df[int_to_numeric], as.numeric)

# Check the data type of each variable after conversion
sapply(df, class)

# REMOVING NEAR ZERO VARIANCE
# Check for near zero variance variables and drop them
low_var_col <- nearZeroVar(df, names = TRUE)
low_var_col
df <- df[, !(names(df) %in% low_var_col)]
dim(df)

# OUTLIERS HANDLING
# Save only numeric columns into numeric_columns
numeric_columns <- sapply(df, is.numeric)
# Convert data into long-format data
df_long <- melt(df[, numeric_columns])

# Visualize numeric data with boxplots
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Variables", y = "Values", title = "Boxplots of Numeric Variables") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Check for outliers using IQR rule
for (col in names(df)[numeric_columns]) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  LB <- Q1 - 1.5 * IQR
  UB <- Q3 + 1.5 * IQR
  outliers <- df[[col]] < LB | df[[col]] > UB
  outlier_count <- sum(outliers, na.rm = TRUE)
  cat(paste("Column:", col, "- Outliers:", outlier_count, "\n"))
}

# Specify outlier columns we want to handle
outlier_cols_handle <- c("WAGP", "PERNP", "PINCP")

# Handle the outliers based on IQR (assign them to the lower or upper bound)
for (col in outlier_cols_handle) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  LB <- Q1 - 1.5 * IQR
  UB <- Q3 + 1.5 * IQR
  df[[col]][df[[col]] < LB] <- LB
  df[[col]][df[[col]] > UB] <- UB
}

# Visualize numeric data with boxplots after handling the outliers
df_long <- melt(df[, numeric_columns])

ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Variables", y = "Values", title = "Boxplots of Numeric Variables") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# BINNING
# Inspect the numeric columns and bin if necessary
# For JWMNP
# Plot the pre-binned histogram
hist(df$JWMNP, 
     main = "Distribution of JWMNP", 
     xlab = "Travel Time to Work (minutes)")

# Bin the values
df$JWMNP <- ifelse(df$JWMNP == 0, 0, # Not a worker or worked from home
                   ifelse(df$JWMNP >= 1 & df$JWMNP <= 10, 1, # Short commute
                          ifelse(df$JWMNP > 10 & df$JWMNP <= 30, 2, # Moderate commute
                                 ifelse(df$JWMNP > 30 & df$JWMNP <= 60, 3, # Long commute 
                                        ifelse(df$JWMNP > 60 & df$JWMNP <= 100, 4, # Very long commute
                                               ifelse(df$JWMNP > 100 & df$JWMNP <= 200, 5, # Extended commute
                                                      ifelse(df$JWMNP == 888, 6, NA))))))) # Suppressed data

# Convert to factor
df$JWMNP <- factor(df$JWMNP, 
                   levels = 0:6) 

# Plot barplot after binning
barplot(table(df$JWMNP), 
        main = "Binned JWMNP", 
        xlab = "Travel Time Category",
        las = 2)

# For WAGP
# Plot the pre-binned histogram
hist(df$WAGP, 
     main = "Distribution of WAGP", 
     xlab = "Wages or Salary Income")

# Bin the values
df$WAGP <- ifelse(df$WAGP == 0, 0, # No income
                  ifelse(df$WAGP <= 20000, 1, # Low income
                         ifelse(df$WAGP <= 50000, 2, # Lower-middle income
                                ifelse(df$WAGP <= 100000, 3, # Middle income 
                                       ifelse(df$WAGP <= 150000, 4, 5))))) # Upper-middle income / High income

# Convert to factor
df$WAGP <- factor(df$WAGP, 
                  levels = 0:5) 

# Plot barplot after binning
barplot(table(df$WAGP), 
        main = "Binned WAGP", 
        xlab = "Wages or Salary Income Category",
        las = 2)

#For WKHP
# Plot the pre-binned histogram
hist(df$WKHP, 
     main = "Distribution of WKHP", 
     xlab = "Usual Hours Worked Per Week")

# Bin the values
df$WKHP <- ifelse(df$WKHP == 0, 0,# Less than 16 years old/did not work during the past 12 months
                  ifelse(df$WKHP >= 1 & df$WKHP <= 20, 1,# Between 1 and 20 hours
                         ifelse(df$WKHP > 20 & df$WKHP <= 40, 2,# Between 20 and 40 hours
                                ifelse(df$WKHP > 40 & df$WKHP <= 60, 3,# Between 40 and 60 hours
                                       ifelse(df$WKHP > 60 & df$WKHP <= 98, 4,# Between 60 and 98 hours
                                              ifelse(df$WKHP >= 99, 5, NA))))))# 99 or more hours

# Convert to factor
df$WKHP <- factor(df$WKHP, 
                  levels = 0:5) 

# Plot barplot after binning
barplot(table(df$WKHP), 
        main = "Binned WKHP", 
        xlab = "Usual Hours Worked Per Week Category",
        las = 2)

#For WKWN
# Plot the pre-binned histogram
hist(df$WKWN, 
     main = "Distribution of WKWN", 
     xlab = "Weeks Worked")

# Bin the values
df$WKWN <- ifelse(df$WKWN == 0, 0,# Less than 16 years old/did not work during the past 12 months
                  ifelse(df$WKWN >= 1 & df$WKWN <= 13, 1,# 1-13 weeks (1st quarter)
                         ifelse(df$WKWN > 13 & df$WKWN <= 26, 2,# 14-26 weeks (2nd quarter)
                                ifelse(df$WKWN > 26 & df$WKWN <= 39, 3,# 27-39 weeks (3rd quarter)
                                       ifelse(df$WKWN > 39 & df$WKWN <= 51, 4,# 40-51 weeks (almost full year)
                                              ifelse(df$WKWN == 52, 5, NA))))))# 52 weeks (full year)

# Convert to factor
df$WKWN <- factor(df$WKWN, 
                  levels = 0:5) 

# Plot barplot after binning
barplot(table(df$WKWN), 
        main = "Binned WKWN", 
        xlab = "Weeks Worked Category",
        las = 2)

#For PERNP
# Plot the pre-binned histogram
hist(df$PERNP, 
     main = "Distribution of PERNP", 
     xlab = "Total Person's Earnings")

# Bin the values
df$PERNP<- ifelse(df$PERNP == 0,0,# No earnings
                  ifelse(df$PERNP == -10000, 1,# Significant loss
                         ifelse(df$PERNP >= -9999 & df$PERNP <= -1, 2,# Small losses
                                ifelse(df$PERNP >= 1 & df$PERNP <= 10000, 3,# Low earners
                                       ifelse(df$PERNP > 10000 & df$PERNP <= 50000, 4,# Middle earners
                                              ifelse(df$PERNP > 50000 & df$PERNP <= 200000, 5,# High earners
                                                     ifelse(df$PERNP > 200000, 6, NA)))))))# Very high earners

# Convert to factor
df$PERNP <- factor(df$PERNP,
                   levels = 0:6) 

# Plot barplot after binning
barplot(table(df$PERNP), 
        main = "Binned PERNP", 
        xlab = "Total Person's Earnings Category",
        las = 2)

#For PINCP
# Plot the pre-binned histogram
hist(df$PINCP, 
     main = "Distribution of PINCP", 
     xlab = "Total Person's Income")

# Bin the values
df$PINCP <- ifelse(df$PINCP == 0, 0,# No income
                   ifelse(df$PINCP == -19998, 1,# Loss of $19,998 or more
                          ifelse(df$PINCP >= -19997 & df$PINCP <= -1, 2,# Losses between $1 and $19,997
                                 ifelse(df$PINCP >= 1 & df$PINCP <= 10000, 3,# Low income
                                        ifelse(df$PINCP > 10000 & df$PINCP <= 50000, 4,# Lower middle income
                                               ifelse(df$PINCP > 50000 & df$PINCP <= 100000, 5,# Middle to upper middle income
                                                      ifelse(df$PINCP > 100000 & df$PINCP <= 4209995, 6, NA))))))) # High income

# Convert to factor
df$PINCP <- factor(df$PINCP,
                   levels = 0:6) 

# Plot barplot after binning
barplot(table(df$PINCP), 
        main = "Binned PINCP", 
        xlab = "Total Person's Income Category",
        las = 2)

#For POVPIP
# Plot the pre-binned histogram
hist(df$POVPIP, 
     main = "Distribution of POVPIP", 
     xlab = "Income-to-Poverty Ratio")

# Bin the values
df$POVPIP <- ifelse(df$POVPIP == -1, 0,# Under 15 years
                    ifelse(df$POVPIP >= 0 & df$POVPIP <= 99, 1,# 0 to 99% of the poverty level
                           ifelse(df$POVPIP >= 100 & df$POVPIP <= 199, 2,# 100 to 199% of the poverty level
                                  ifelse(df$POVPIP >= 200 & df$POVPIP <= 299, 3,# 200 to 299% of the poverty level
                                         ifelse(df$POVPIP >= 300 & df$POVPIP <= 399, 4,# 300 to 399% of the poverty level
                                                ifelse(df$POVPIP >= 400 & df$POVPIP <= 500, 5,# 400 to 500% of the poverty level
                                                       ifelse(df$POVPIP == 501, 6, NA)))))))# 501% or more of the poverty level

# Convert to factor
df$POVPIP <- factor(df$POVPIP,
                    levels = 0:6) 

# Plot barplot after binning
barplot(table(df$POVPIP), 
        main = "Binned POVPIP", 
        xlab = "Income-to-Poverty Ratio Category",
        las = 2)

# CLASS DISTRIBUTION
# Check how the classes are distributed
table(df$Class)
# Check how the classes are distributed (% wise)
prop.table(table(df$Class))

# Visualize class distribution
ggplot(df, aes(x = Class, fill = Class)) +
  geom_bar() +
  scale_fill_manual(values = c("steelblue", "orange")) +
  theme_minimal() +
  labs(title = "Class Distribution", x = "Class", y = "Count") +
  theme(legend.title = element_blank())

# COLLINEARITY
# Select only numeric columns
numeric_columns <- sapply(df, is.numeric)
numeric_df <- df[, numeric_columns]

# Compute correlation matrix
corr <- cor(numeric_df)

# Visualize correlations
corrplot(corr, method = "color", type = "upper", tl.cex = 0.7)

# Find highly correlated features (cutoff 0.7)
highCorr <- findCorrelation(corr, cutoff = 0.7, names = TRUE)
cat("Highly Correlated Features:", highCorr, "\n")

# SAVE PREPROCESSED DATASET
write.csv(df, "preprocessed_data.csv", row.names = FALSE)






install.packages("caTools")
library(caTools)

# Load the preprocessed dataset
df_prep <- df
df_prep
# Function to convert all columns to numeric except 'Class'
convert_columns_to_numeric <- function(data, exclude_col = "Class") {
  # Identify columns to convert (all except 'Class')
  cols_to_convert <- setdiff(names(data), exclude_col)
  
  # Convert selected columns to numeric
  for (col in cols_to_convert) {
    data[[col]] <- as.numeric(as.factor(data[[col]]))# Convert to factor first, then numeric
  }
  
  return(data)
}

# Apply function to df_prep
df_prep <- convert_columns_to_numeric(df_prep, exclude_col = "Class")

# Set seed for reproducibility
set.seed(123)

# Create a sample split (80% training, 20% testing)
split <- sample.split(df_prep$Class, SplitRatio = 0.8)

# Create training and testing datasets
train_data <- subset(df_prep, split == TRUE)
test_data <- subset(df_prep, split == FALSE)

# Check the dimensions of train and test datasets
dim(train_data)
dim(test_data)

# Balancing Method 1 (Oversampling)
install.packages("ROSE")
library(ROSE)

class_counts <- table(train_data$Class)
class_counts
max_class_size <- max(class_counts)

# Apply SMOTE: Oversample minority class to match majority class
train_data_oversampled <- ovun.sample(Class ~ ., 
                                      data = train_data, 
                                      method = "over", 
                                      N = max_class_size * length(unique(train_data$Class)), 
                                      seed = 123)$data

# Verify the class distribution
table(train_data_oversampled$Class)


# Balancing Method 2 (Undersampling)
# Get the class distribution
min_class_size <- min(class_counts)# Find the minority class count

# Apply undersampling: Reduce majority class to match the minority class
train_data_undersampled <- ovun.sample(Class ~ ., 
                                       data = train_data, 
                                       method = "under", 
                                       N = min_class_size * length(unique(train_data$Class)), 
                                       seed = 123)$data

# Verify the class distribution
table(train_data_undersampled$Class)

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



# Create datasets
# Select only the columns in selected_features_info_gain_oversampled
# Select only the chosen features + Class column
df1 <- train_data_oversampled[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]# Info Gain training Dataset - oversampled
dim(df1)
df1_test <- test_data[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]#info gain test dataset -oversampled
dim(df1_test)

df2 <- train_data_undersampled[, c(selected_features_info_gain_undersampled, "Class"), drop = FALSE]#info gain training dataset - undersampled
dim(df2)
df2_test <- test_data[, c(selected_features_info_gain_undersampled, "Class"), drop = FALSE]#info gain test dataset - undersampled
dim(df2_test)

df3 <- train_data_oversampled[, c(selected_features_boruta_oversampled, "Class"), drop = FALSE]#boruta training dataset -oversampled
dim(df3)
df3_test <- test_data[, c(selected_features_boruta_oversampled, "Class"), drop = FALSE]#boruta test dataset - oversampled
dim(df3_test)

df4 <- train_data_undersampled[, c(selected_features_boruta_undersampled, "Class"), drop = FALSE]#boruta training dataset - undersampled
dim(df4)
df4_test <- test_data[, c(selected_features_boruta_undersampled, "Class"), drop = FALSE]#boruta test dataset - undersampled
dim(df4_test)

df5 <- train_data_oversampled[, c(selected_features_lasso_oversampled, "Class"), drop = FALSE]#lasso training dataset -oversampled
dim(df5)
df5_test <- test_data[, c(selected_features_lasso_oversampled, "Class"), drop = FALSE]#lasso test dataset - oversampled
dim(df5_test)

df6 <- train_data_undersampled[, c(selected_features_lasso_undersampled, "Class"), drop = FALSE]#lasso training dataset -undersampled
dim(df6)
df6_test <- test_data[, c(selected_features_lasso_undersampled, "Class"), drop = FALSE]#lasso test dataset - undersampled
dim(df6_test)

# Build models
# Logistic Regression


# Install required packages if not already installed
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("pROC")) install.packages("pROC", dependencies = TRUE)
if (!require("mltools")) install.packages("mltools", dependencies = TRUE)
if (!require("Metrics")) install.packages("Metrics", dependencies = TRUE)

library(caret)
library(pROC)
library(mltools)
library(Metrics)

# Function to evaluate model using test data
evaluate_model <- function(model, test_data, model_name) {
  
  # Predict probabilities and class labels on test data
  predictions_prob <- predict(model, newdata = test_data, type = "response")
  predictions <- ifelse(predictions_prob > 0.5, "Yes", "No")
  
  # Convert to factors for confusion matrix
  actual <- as.factor(test_data$Class)
  predicted <- as.factor(predictions)
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual, positive = "Yes")
  print(paste("Confusion Matrix for", model_name))
  print(conf_matrix$table)
  
  # Extract performance metrics
  results <- data.frame(
    Metric = c("True Positive Rate (Recall)", "False Positive Rate", "Precision", 
               "F1 Score", "ROC AUC", "MCC", "Kappa"),
    Yes = c(
      conf_matrix$byClass["Sensitivity"], 
      1 - conf_matrix$byClass["Specificity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      as.numeric(roc(actual, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    No = c(
      conf_matrix$byClass["Specificity"], 
      1 - conf_matrix$byClass["Sensitivity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      NA,  # ROC AUC is the same for both classes
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    Weighted_Avg = c(
      mean(c(conf_matrix$byClass["Sensitivity"], conf_matrix$byClass["Specificity"])), 
      mean(c(1 - conf_matrix$byClass["Specificity"], 1 - conf_matrix$byClass["Sensitivity"])), 
      mean(c(conf_matrix$byClass["Precision"], conf_matrix$byClass["Precision"])), 
      mean(c(conf_matrix$byClass["F1"], conf_matrix$byClass["F1"])), 
      as.numeric(roc(actual, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    )
  )
  
  print(paste("Performance Metrics for", model_name))
  print(results)
  
  return(results)
}

# Train models on training data
model_df1 <- glm(Class ~ ., data = df1, family = binomial)
model_df2 <- glm(Class ~ ., data = df2, family = binomial)
model_df3 <- glm(Class ~ ., data = df3, family = binomial)
model_df4 <- glm(Class ~ ., data = df4, family = binomial)
model_df5 <- glm(Class ~ ., data = df5, family = binomial)
model_df6 <- glm(Class ~ ., data = df6, family = binomial)

# Evaluate models using corresponding test datasets
metrics_df1 <- evaluate_model(model_df1, df1_test, "df1 (Info Gain Oversampled)")
metrics_df2 <- evaluate_model(model_df2, df2_test, "df2 (Info Gain Undersampled)")
metrics_df3 <- evaluate_model(model_df3, df3_test, "df3 (Boruta Oversampled)")
metrics_df4 <- evaluate_model(model_df4, df4_test, "df4 (Boruta Undersampled)")
metrics_df5 <- evaluate_model(model_df5, df5_test, "df5 (LASSO Oversampled)")
metrics_df6 <- evaluate_model(model_df6, df6_test, "df6 (LASSO Undersampled)")