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




# MODELING

install.packages("caTools")
library(caTools)
# Function to convert all columns to numeric except 'Class'
cols_to_numeric <- function(data, exclude_col = "Class") {
  cols_to_convert <- setdiff(names(data), exclude_col)
  for (col in cols_to_convert) {
    data[[col]] <- as.numeric(data[[col]])  # Convert to factor first, then numeric
  }
  return(data)
}

# Apply cols_to_numeric function to df
df <- cols_to_numeric(df, exclude_col = "Class")

# Convert Class column: "Yes" -> 1, "No" -> 0 and ensure it's numeric
df$Class <- ifelse(df$Class == "Yes", 1, 0)
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
set.seed(324)

# Create a training/testing split (80% training, 20% testing)
split <- sample.split(df$Class, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Check the dimensions of train and test datasets
dim(train_data)
dim(test_data)

# Balancing Method 1 (Oversampling)
# SMOTE
# Install and load the package
install.packages("smotefamily")
library(smotefamily)

# Separate features and labels
X <- train_data[, setdiff(names(train_data), "Class")]
y <- train_data$Class

# Count instances per class
class_counts <- table(y)
class_counts
majority_class_size <- max(class_counts)

# Calculate duplication size
dup_sizes <- sapply(class_counts, function(n) {
  if (n < majority_class_size) {
    ceiling((majority_class_size - n) / n)
  } else {
    0
  }
})

# Apply SMOTE separately for each minority class
smote_list <- list()

for (class_name in names(dup_sizes)) {
  if (dup_sizes[class_name] > 0) {
    class_data <- X[y == class_name, ]
    class_labels <- y[y == class_name]
    smote_result <- SMOTE(class_data, class_labels, K = 5, dup_size = dup_sizes[class_name])
    synthetic <- smote_result$syn_data
    synthetic$class <- class_name
    smote_list[[class_name]] <- synthetic
  }
}

# Combine all synthetic data
synthetic_data <- do.call(rbind, smote_list)

# Fix column name: 'class' → 'Class'
names(synthetic_data)[names(synthetic_data) == "class"] <- "Class"

# Reorder columns to match train_data
synthetic_data <- synthetic_data[, names(train_data)]

# Combine original and synthetic data
train_data_oversampled <- rbind(train_data, synthetic_data)

# Ensure Class is a factor
train_data_oversampled$Class <- as.factor(train_data_oversampled$Class)

# Check final class balance
table(train_data_oversampled$Class)

# Balancing Method 2 (Undersampling)
# Get the class distribution
install.packages("ROSE")
library(ROSE)
minority_class_size <- min(class_counts)  # Find the minority class count
min_class_size <- min(class_counts)# Find the minority class count

# Apply undersampling: Reduce majority class to match the minority class
train_data_undersampled <- ovun.sample(Class ~ ., 
                                       data = train_data, 
                                       method = "under", 
                                       N = minority_class_size * length(unique(train_data$Class)), 
                                       seed = 123)$data

# Ensure Class is a factor
train_data_undersampled$Class <- as.factor(train_data_undersampled$Class)

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

N <- 15# Keep top 15 features based on the elbow
selected_features_info_gain_oversampled <- sorted_info_gain_oversampled[1:N, "attributes"]
length(selected_features_info_gain_oversampled)


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
length(selected_features_info_gain_undersampled)

# Feature Selection Method 2 (Boruta)
install.packages("Boruta")
library(Boruta)

# For oversampled
# Run Boruta for feature selection
set.seed(324)# Ensure reproducibility
boruta_result_oversampled <- Boruta(Class ~ ., data = train_data_oversampled, doTrace = 2)
boruta_result_oversampled
# Plot Boruta feature selection results
plot(boruta_result_oversampled, las = 2, cex.axis = 0.7)

# Get final feature importance decision
selected_features_boruta_oversampled <- getSelectedAttributes(boruta_result_oversampled, withTentative = FALSE)
length(selected_features_boruta_oversampled)

# For undersampled
# Run Boruta for feature selection
set.seed(324)# Ensure reproducibility
boruta_result_undersampled <- Boruta(Class ~ ., data = train_data_undersampled, doTrace = 2)
boruta_result_undersampled
# Plot Boruta feature selection results
plot(boruta_result_undersampled, las = 2, cex.axis = 0.7)
# Resolve tentative attributes
boruta_result_fixed_undersampled <- TentativeRoughFix(boruta_result_undersampled)
# Extract confirmed features after resolving tentatives
selected_features_boruta_undersampled <- getSelectedAttributes(boruta_result_fixed_undersampled)
length(selected_features_boruta_undersampled)

# Feature Selection Method 3 (Lasso Regularization)
# Load necessary packages
install.packages("glmnet")
library(glmnet)

# Function to apply LASSO on mixed numeric and factor data
apply_lasso_feature_selection <- function(data) {
  
  # Ensure Class is a factor
  data$Class <- as.factor(data$Class)
  
  # Convert factor variables to numeric (direct mapping)
  factor_columns <- sapply(data, is.factor)  # Identify factor columns
  data[factor_columns] <- lapply(data[factor_columns], as.numeric)  # Convert factors to numbers
  
  # Separate features (X) and target (y)
  X <- as.matrix(data[, -which(names(data) == "Class")])  # Exclude target
  y <- as.numeric(data$Class) - 1  # Convert target to binary (0/1)

  factor_columns <- sapply(data, is.factor)# Identify factor columns
  data[factor_columns] <- lapply(data[factor_columns], as.numeric)# Convert factors to numbers
  
  # Separate features (X) and target (y)
  X <- as.matrix(data[, -which(names(data) == "Class")])# Exclude target
  y <- as.numeric(data$Class) - 1# Convert target to binary (0/1)
  
  # Fit cross-validated Lasso logistic regression
  set.seed(324)# For reproducibility
  cv_model <- cv.glmnet(
    X, 
    y, 
    family = "binomial",  # For binary classification
    alpha = 1,            # Lasso penalty (L1 regularization)
    type.measure = "class",  # Classification error as metric
    standardize = TRUE   # Standardize features
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
length(selected_features_lasso_oversampled)
selected_features_lasso_oversampled


# Apply function to undersampled data
lasso_undersampled <- apply_lasso_feature_selection(train_data_undersampled)
selected_features_lasso_undersampled <- lasso_undersampled$selected_features
length(selected_features_lasso_undersampled)
selected_features_lasso_undersampled




# Create datasets
# Select only the columns in selected_features_info_gain_oversampled + Class column
df1 <- train_data_oversampled[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]
dim(df1)
df1_test <- test_data[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]
dim(df1_test)

df2 <- train_data_undersampled[, c(selected_features_info_gain_undersampled, "Class"), drop = FALSE]
dim(df2)
df2_test <- test_data[, c(selected_features_info_gain_undersampled, "Class"), drop = FALSE]
dim(df2_test)

df3 <- train_data_oversampled[, c(selected_features_boruta_oversampled, "Class"), drop = FALSE]
dim(df3)
df3_test <- test_data[, c(selected_features_boruta_oversampled, "Class"), drop = FALSE]
dim(df3_test)

df4 <- train_data_undersampled[, c(selected_features_boruta_undersampled, "Class"), drop = FALSE]
dim(df4)
df4_test <- test_data[, c(selected_features_boruta_undersampled, "Class"), drop = FALSE]
dim(df4_test)

df5 <- train_data_oversampled[, c(selected_features_lasso_oversampled, "Class"), drop = FALSE]
dim(df5)
df5_test <- test_data[, c(selected_features_lasso_oversampled, "Class"), drop = FALSE]
dim(df5_test)

df6 <- train_data_undersampled[, c(selected_features_lasso_undersampled, "Class"), drop = FALSE]
dim(df6)
df6_test <- test_data[, c(selected_features_lasso_undersampled, "Class"), drop = FALSE]
dim(df6_test)

df1_test$Class

# Build models
# Logistic Regression

# Select only the columns in selected_features_info_gain_oversampled
# Select only the chosen features + Class column
df1 <- train_data_oversampled[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]# Info Gain training Dataset - oversampled
dim(df1)
df1_test <- test_data[, c(selected_features_info_gain_oversampled, "Class"), drop = FALSE]#info gain test dataset -oversampled
dim(df1_test)
df1_test$Class
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


# XGBoost
# Install required packages if not already installed
if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
library(xgboost)

# Convert datasets for XGBoost
prepare_xgb_data <- function(df_train, df_test) {
  df_train$Class <- as.numeric(df_train$Class) - 1  # Convert to 0/1
  df_test$Class <- as.numeric(df_test$Class) - 1  # Convert to 0/1
  
  train_matrix <- xgb.DMatrix(data = as.matrix(df_train[, -which(names(df_train) == "Class")]), label = df_train$Class)
  test_matrix <- xgb.DMatrix(data = as.matrix(df_test[, -which(names(df_test) == "Class")]), label = df_test$Class)
  
  return(list(train = train_matrix, test = test_matrix, test_labels = df_test$Class))
}

# Prepare all datasets
df1_xgb <- prepare_xgb_data(df1, df1_test)
df2_xgb <- prepare_xgb_data(df2, df2_test)
df3_xgb <- prepare_xgb_data(df3, df3_test)
df4_xgb <- prepare_xgb_data(df4, df4_test)
df5_xgb <- prepare_xgb_data(df5, df5_test)
df6_xgb <- prepare_xgb_data(df6, df6_test)

train_xgb_model <- function(train_matrix) {
  set.seed(324)  # Ensure reproducibility
  
  # Define XGBoost parameters
  params <- list(
    objective = "binary:logistic",  # Binary classification
    eval_metric = "auc",  # Optimize for AUC
    max_depth = 6,  # Tree depth
    eta = 0.1,  # Learning rate
    subsample = 0.8,  # Reduce overfitting
    colsample_bytree = 0.8  # Feature sampling
  )
  
  # Train the model
  xgb_model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 100,  # Number of boosting rounds
    verbose = 0  # Silent training
  )
  
  return(xgb_model)
}

model_xgb_df1 <- train_xgb_model(df1_xgb$train)
model_xgb_df2 <- train_xgb_model(df2_xgb$train)
model_xgb_df3 <- train_xgb_model(df3_xgb$train)
model_xgb_df4 <- train_xgb_model(df4_xgb$train)
model_xgb_df5 <- train_xgb_model(df5_xgb$train)
model_xgb_df6 <- train_xgb_model(df6_xgb$train)

evaluate_xgb_model <- function(model, test_matrix, test_labels, model_name) {
  
  # Predict probabilities
  predictions_prob <- predict(model, newdata = test_matrix)
  predictions <- ifelse(predictions_prob > 0.5, 1, 0)  # Convert to binary labels
  
  # Convert actual and predicted to factors
  actual <- as.factor(test_labels)
  predicted <- as.factor(predictions)
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual, positive = "1")
  print(paste("Confusion Matrix for", model_name))
  print(conf_matrix$table)
  
  # Extract performance metrics
  results <- data.frame(
    Metric = c("True Positive Rate (Recall)", "False Positive Rate", "Precision", 
               "F1 Score", "ROC AUC", "Kappa"),
    Class_1 = c(
      conf_matrix$byClass["Sensitivity"], 
      1 - conf_matrix$byClass["Specificity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      as.numeric(roc(test_labels, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    Class_0 = c(
      conf_matrix$byClass["Specificity"], 
      1 - conf_matrix$byClass["Sensitivity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      NA,  # ROC AUC is the same for both classes
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    Weighted_Avg = c(
      mean(c(conf_matrix$byClass["Sensitivity"], conf_matrix$byClass["Specificity"])), 
      mean(c(1 - conf_matrix$byClass["Specificity"], 1 - conf_matrix$byClass["Sensitivity"])), 
      mean(c(conf_matrix$byClass["Precision"], conf_matrix$byClass["Precision"])), 
      mean(c(conf_matrix$byClass["F1"], conf_matrix$byClass["F1"])), 
      as.numeric(roc(test_labels, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    )
  )
  
  print(paste("Performance Metrics for", model_name))
  print(results)
  
  return(results)
}

metrics_xgb_df1 <- evaluate_xgb_model(model_xgb_df1, df1_xgb$test, df1_xgb$test_labels, "df1 (Info Gain Oversampled)")
metrics_xgb_df2 <- evaluate_xgb_model(model_xgb_df2, df2_xgb$test, df2_xgb$test_labels, "df2 (Info Gain Undersampled)")
metrics_xgb_df3 <- evaluate_xgb_model(model_xgb_df3, df3_xgb$test, df3_xgb$test_labels, "df3 (Boruta Oversampled)")
metrics_xgb_df4 <- evaluate_xgb_model(model_xgb_df4, df4_xgb$test, df4_xgb$test_labels, "df4 (Boruta Undersampled)")
metrics_xgb_df5 <- evaluate_xgb_model(model_xgb_df5, df5_xgb$test, df5_xgb$test_labels, "df5 (LASSO Oversampled)")
metrics_xgb_df6 <- evaluate_xgb_model(model_xgb_df6, df6_xgb$test, df6_xgb$test_labels, "df6 (LASSO Undersampled)")


if (!require("neuralnet")) install.packages("neuralnet", dependencies = TRUE)
library(neuralnet)

# Normalize the data (Min-Max Scaling)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prepare_nn_data <- function(df_train, df_test) {
  df_train$Class <- as.numeric(df_train$Class) - 1  # Convert to 0/1
  df_test$Class <- as.numeric(df_test$Class) - 1  # Convert to 0/1
  
  # Apply normalization to all columns except Class
  df_train[, -which(names(df_train) == "Class")] <- as.data.frame(lapply(df_train[, -which(names(df_train) == "Class")], normalize))
  df_test[, -which(names(df_test) == "Class")] <- as.data.frame(lapply(df_test[, -which(names(df_test) == "Class")], normalize))
  
  return(list(train = df_train, test = df_test))
}

# Prepare datasets
df1_nn <- prepare_nn_data(df1, df1_test)
df2_nn <- prepare_nn_data(df2, df2_test)
df3_nn <- prepare_nn_data(df3, df3_test)
df4_nn <- prepare_nn_data(df4, df4_test)
df5_nn <- prepare_nn_data(df5, df5_test)
df6_nn <- prepare_nn_data(df6, df6_test)

train_nn_model <- function(train_data) {
  set.seed(324)  # Ensure reproducibility
  
  # Define formula for neural network (Class ~ all features)
  feature_names <- names(train_data)[-which(names(train_data) == "Class")]
  formula <- as.formula(paste("Class ~", paste(feature_names, collapse = " + ")))
  
  # Train the neural network
  nn_model <- neuralnet(
    formula, 
    data = train_data, 
    hidden = c(round((ncol(train_data) - 1) / 2)),  # 1 hidden layer
    linear.output = FALSE, 
    stepmax = 1e6
  )
  
  return(nn_model)
}

model_nn_df1 <- train_nn_model(df1_nn$train)
model_nn_df2 <- train_nn_model(df2_nn$train)
model_nn_df3 <- train_nn_model(df3_nn$train)
model_nn_df4 <- train_nn_model(df4_nn$train)
model_nn_df5 <- train_nn_model(df5_nn$train)
model_nn_df6 <- train_nn_model(df6_nn$train)


evaluate_nn_model <- function(model, test_data, model_name) {
  
  # Predict on test data
  predictions_prob <- compute(model, test_data[, -which(names(test_data) == "Class")])$net.result
  predictions <- ifelse(predictions_prob > 0.5, 1, 0)  # Convert to binary labels
  
  # Convert actual and predicted to factors
  actual <- as.factor(test_data$Class)
  predicted <- as.factor(predictions)
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual, positive = "1")
  print(paste("Confusion Matrix for", model_name))
  print(conf_matrix$table)
  
  # Extract performance metrics
  results <- data.frame(
    Metric = c("True Positive Rate (Recall)", "False Positive Rate", "Precision", 
               "F1 Score", "ROC AUC", "MCC", "Kappa"),
    Class_1 = c(
      conf_matrix$byClass["Sensitivity"], 
      1 - conf_matrix$byClass["Specificity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      as.numeric(roc(test_data$Class, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    Class_0 = c(
      conf_matrix$byClass["Specificity"], 
      1 - conf_matrix$byClass["Sensitivity"], 
      conf_matrix$byClass["Precision"], 
      conf_matrix$byClass["F1"], 
      NA,  # ROC AUC is the same for both classes
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    ),
    Weighted_Avg = c(
      mean(c(conf_matrix$byClass["Sensitivity"], conf_matrix$byClass["Specificity"])), 
      mean(c(1 - conf_matrix$byClass["Specificity"], 1 - conf_matrix$byClass["Sensitivity"])), 
      mean(c(conf_matrix$byClass["Precision"], conf_matrix$byClass["Precision"])), 
      mean(c(conf_matrix$byClass["F1"], conf_matrix$byClass["F1"])), 
      as.numeric(roc(test_data$Class, predictions_prob)$auc), 
      mcc(actual, predicted), 
      conf_matrix$overall["Kappa"]
    )
  )
  
  print(paste("Performance Metrics for", model_name))
  print(results)
  
  return(results)
}


metrics_nn_df1 <- evaluate_nn_model(model_nn_df1, df1_nn$test, "df1 (Info Gain Oversampled)")
metrics_nn_df2 <- evaluate_nn_model(model_nn_df2, df2_nn$test, "df2 (Info Gain Undersampled)")
metrics_nn_df3 <- evaluate_nn_model(model_nn_df3, df3_nn$test, "df3 (Boruta Oversampled)")
metrics_nn_df4 <- evaluate_nn_model(model_nn_df4, df4_nn$test, "df4 (Boruta Undersampled)")
metrics_nn_df5 <- evaluate_nn_model(model_nn_df5, df5_nn$test, "df5 (LASSO Oversampled)")
metrics_nn_df6 <- evaluate_nn_model(model_nn_df6, df6_nn$test, "df6 (LASSO Undersampled)")


install.packages("mltools")

# Load required packages
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(naivebayes)
library(mltools)



# Updated function to calculate performance metrics including AUC
calculate_metrics <- function(cm, auc_value) {
  # Extract values from the confusion matrix
  tp_1 <- as.numeric(cm["1", "1"]) # True Positive for class 1
  fp_1 <- as.numeric(cm["0", "1"]) # False Positive for class 1
  tn_1 <- as.numeric(cm["0", "0"]) # True Negative for class 1
  fn_1 <- as.numeric(cm["1", "0"]) # False Negative for class 1
  
  tp_0 <- as.numeric(cm["0", "0"]) # True Positive for class 0
  fp_0 <- as.numeric(cm["1", "0"]) # False Positive for class 0
  tn_0 <- as.numeric(cm["1", "1"]) # True Negative for class 0
  fn_0 <- as.numeric(cm["0", "1"]) # False Negative for class 0
  
  # Calculate performance metrics for class 1
  tpr_1 <- tp_1 / (tp_1 + fn_1) # Sensitivity, Recall, or True Positive Rate (TPR)
  fpr_1 <- fp_1 / (fp_1 + tn_1) # False Positive Rate (FPR)
  precision_1 <- tp_1 / (tp_1 + fp_1) # Precision
  recall_1 <- tpr_1 # Same as TPR
  f_measure_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1) # F1-score
  mcc_1 <- ((tp_1 * tn_1) - (fp_1 * fn_1)) / 
    sqrt((tp_1 + fp_1) * (tp_1 + fn_1) * (tn_1 + fp_1) * (tn_1 + fn_1)) # MCC
  kappa_1 <- 2 * (tp_1 * tn_1 - fp_1 * fn_1) / 
    ((tp_1 + fn_1) * (fn_1 + tn_1) + (tp_1 + fp_1) * (fp_1 + tn_1)) # Kappa
  
  # Calculate performance metrics for class 0
  tpr_0 <- tp_0 / (tp_0 + fn_0)
  fpr_0 <- fp_0 / (fp_0 + tn_0)
  precision_0 <- tp_0 / (tp_0 + fp_0)
  recall_0 <- tpr_0
  f_measure_0 <- 2 * (precision_0 * recall_0) / (precision_0 + recall_0)
  mcc_0 <- ((tp_0 * tn_0) - (fp_0 * fn_0)) / 
    sqrt((tp_0 + fp_0) * (tp_0 + fn_0) * (tn_0 + fp_0) * (tn_0 + fn_0))
  kappa_0 <- 2 * (tp_0 * tn_0 - fp_0 * fn_0) / 
    ((tp_0 + fn_0) * (fn_0 + tn_0) + (tp_0 + fp_0) * (fp_0 + tn_0))
  
  # Simple average for class-wise metrics
  avg_tpr <- (tpr_1 + tpr_0) / 2
  avg_fpr <- (fpr_1 + fpr_0) / 2
  avg_precision <- (precision_1 + precision_0) / 2
  avg_recall <- (recall_1 + recall_0) / 2
  avg_f_measure <- (f_measure_1 + f_measure_0) / 2
  avg_mcc <- (mcc_1 + mcc_0) / 2
  avg_kappa <- (kappa_1 + kappa_0) / 2
  
  # Create performance metrics table
  performance_table <- data.frame(
    Class = c("Class 1", "Class 0", "Simple Avg"),
    TPR = c(tpr_1, tpr_0, avg_tpr),
    FPR = c(fpr_1, fpr_0, avg_fpr),
    Precision = c(precision_1, precision_0, avg_precision),
    Recall = c(recall_1, recall_0, avg_recall),
    F_measure = c(f_measure_1, f_measure_0, avg_f_measure),
    AUC = c(auc_value, auc_value, auc_value),  # Same AUC for all rows
    MCC = c(mcc_1, mcc_0, avg_mcc),
    Kappa = c(kappa_1, kappa_0, avg_kappa)
  )
  
  # Return the performance table
  return(performance_table)
}

#----------------------------------------------------------------RANDOM FOREST----------------------------------------------------------------

# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF1------------------------------------------
set.seed(324)
rf1 <- df1
rf1_test <-df1_test

# Convert the Class variable to factor with correct levels for Random Forest
rf1$Class <- factor(rf1$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf1_test$Class <- factor(rf1_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(15, 20, 40, 50)
)
rf_model1_tuned <- train(
  Class ~ .,  
  data = rf1,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE,
  metric = "Kappa"
)
print(rf_model1_tuned$bestTune)
pred_prob <- predict(rf_model1_tuned,rf1_test,type="prob")
roc_curve <- roc(response = rf1_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
# Generate the confusion matrix
cm <- table(Actual = rf1_test$Class, Predicted = pred)

# Rename the confusion matrix labels back to "0" and "1"
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")

# Print Confusion Matrix
print("Confusion Matrix for df1 using Random Forest:")
print(cm)

# Calculate performance metrics using the same function
print("Performance for df1 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)



# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF2------------------------------------------
set.seed(324)
rf2 <- df2
rf2_test <-df2_test

# Convert the Class variable to factor with correct levels for Random Forest
rf2$Class <- factor(rf2$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf2_test$Class <- factor(rf2_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(15, 20, 40, 50)
)
rf_model2_tuned <- train(
  Class ~ .,  
  data = rf2,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE,
  metric = "Kappa"
)
print(rf_model2_tuned$bestTune)
pred_prob <- predict(rf_model2_tuned,rf2_test,type="prob")
roc_curve <- roc(response = rf2_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
# Generate the confusion matrix
cm <- table(Actual = rf2_test$Class, Predicted = pred)

# Rename the confusion matrix labels back to "0" and "1"
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")

# Print Confusion Matrix
print("Confusion Matrix for df2 using Random Forest:")
print(cm)

# Calculate performance metrics using the same function
print("Performance for df2 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF3------------------------------------------
set.seed(324)
rf3 <- df3
rf3_test <-df3_test

# Convert the Class variable to factor with correct levels for Random Forest
rf3$Class <- factor(rf3$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf3_test$Class <- factor(rf3_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(15, 20, 40, 50)
)
rf_model3_tuned <- train(
  Class ~ .,  
  data = rf3,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE,
  metric = "Kappa"
)
print(rf_model3_tuned$bestTune)
pred_prob <- predict(rf_model3_tuned,rf3_test,type="prob")
roc_curve <- roc(response = rf3_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = rf3_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df3 using Random Forest:")
print(cm)
print("Performance for df3 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF4------------------------------------------
set.seed(324)
rf4 <- df4
rf4_test <-df4_test

# Convert the Class variable to factor with correct levels for Random Forest
rf4$Class <- factor(rf4$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf4_test$Class <- factor(rf4_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(1,5, 10, 15, 20)
)
rf_model4_tuned <- train(
  Class ~ .,  
  data = rf4,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE
)
print(rf_model4_tuned$bestTune)
pred_prob <- predict(rf_model4_tuned,rf4_test,type="prob")
roc_curve <- roc(response = rf4_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = rf4_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df4 using Random Forest:")
print(cm)
print("Performance for df4 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)


# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF5------------------------------------------
set.seed(324)
rf5 <- df5
rf5_test <-df5_test

# Convert the Class variable to factor with correct levels for Random Forest
rf5$Class <- factor(rf5$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf5_test$Class <- factor(rf5_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(10, 20,25, 40,50)
)
rf_model5_tuned <- train(
  Class ~ .,  
  data = rf5,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE,
  metric = "Kappa"
)
print(rf_model5_tuned$bestTune)
pred_prob <- predict(rf_model5_tuned,rf5_test,type="prob")
roc_curve <- roc(response = rf5_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = rf5_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df5 using Random Forest:")
print(cm)
print("Performance for df5 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)


# --------------------------------------------RUN FROM BELOW FOR RANDOM FOREST USING DF6------------------------------------------
set.seed(324)
rf6 <- df6
rf6_test <-df6_test

# Convert the Class variable to factor with correct levels for Random Forest
rf6$Class <- factor(rf6$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
rf6_test$Class <- factor(rf6_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs = TRUE)
tune_grid <- expand.grid(
  mtry = c(15, 20, 40, 50)
)
rf_model6_tuned <- train(
  Class ~ .,  
  data = rf6,  
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  importance = TRUE,
  metric = "Kappa"
)
print(rf_model6_tuned$bestTune)
pred_prob <- predict(rf_model6_tuned,rf6_test,type="prob")
roc_curve <- roc(response = rf6_test$Class, predictor = pred_prob[, "Class1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = rf6_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df6 using Random Forest:")
print(cm)
print("Performance for df6 using Random Forest:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)




#----------------------------------------------------------------KNN---------------------------------------------------------------------


# --------------------------------------------RUN FROM BELOW FOR KNN USING DF1------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(5, 7, 9, 11) 
)

knn_df1_tuned <- train(
  Class ~ ., 
  data = df1,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn
)
print(knn_df1_tuned$bestTune)
pred_prob <- predict(knn_df1_tuned,df1_test,type="prob")
roc_curve <- roc(response=df1_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df1_test$Class, Predicted = pred)
print("Confusion Matrix for df1 using KNN : ")
print(cm)
print("Perfomance for df1 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)



#--------------------------------------------RUN FROM BELOW FOR KNN USING DF2------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11) 
)
knn_df2_tuned <- train(
  Class ~ ., 
  data = df2,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn,
  metric="Kappa"
)
print(knn_df2_tuned$bestTune)
pred_prob <- predict(knn_df2_tuned,df2_test,type="prob")
roc_curve <- roc(response=df2_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df2_test$Class, Predicted = pred)
print("Confusion Matrix for df2 using KNN : ")
print(cm)
print("Perfomance for df2 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

#--------------------------------------------RUN FROM BELOW FOR KNN USING DF3------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11) 
)
knn_df3_tuned <- train(
  Class ~ ., 
  data = df3,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn,
  metric="Kappa"
)
print(knn_df3_tuned$bestTune)
pred_prob <- predict(knn_df3_tuned,df3_test,type="prob")
roc_curve <- roc(response=df3_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df3_test$Class, Predicted = pred)
print("Confusion Matrix for df3 using KNN : ")
print(cm)
print("Perfomance for df3 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)


#--------------------------------------------RUN FROM BELOW FOR KNN USING DF4------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11) 
)
knn_df4_tuned <- train(
  Class ~ ., 
  data = df4,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn,
  metric="Kappa"
)
print(knn_df4_tuned$bestTune)
pred_prob <- predict(knn_df4_tuned,df4_test,type="prob")
roc_curve <- roc(response=df4_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df4_test$Class, Predicted = pred)
print("Confusion Matrix for df4 using KNN : ")
print(cm)
print("Perfomance for df4 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)


#--------------------------------------------RUN FROM BELOW FOR KNN USING DF5------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11) 
)
knn_df5_tuned <- train(
  Class ~ ., 
  data = df5,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn,
  metric="Kappa"
)
print(knn_df5_tuned$bestTune)
pred_prob <- predict(knn_df5_tuned,df5_test,type="prob")
roc_curve <- roc(response=df5_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df5_test$Class, Predicted = pred)
print("Confusion Matrix for df5 using KNN : ")
print(cm)
print("Perfomance for df5 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

#--------------------------------------------RUN FROM BELOW FOR KNN USING DF6------------------------------------------
set.seed(324)
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
tune_grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11) 
)
knn_df6_tuned <- train(
  Class ~ ., 
  data = df6,
  method = "knn",
  trControl = control,
  tuneGrid = tune_grid_knn,
  metric="Kappa"
)
print(knn_df6_tuned$bestTune)
pred_prob <- predict(knn_df6_tuned,df6_test,type="prob")
roc_curve <- roc(response=df5_test$Class,predictor=pred_prob[,"1"]) 
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "1"] > 0.5, "1", "0")
cm <- table(Actual = df6_test$Class, Predicted = pred)
print("Confusion Matrix for df6 using KNN : ")
print(cm)
print("Perfomance for df5 using KNN : ")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

#----------------------------------------------------------------SUPPORT VECTOR MACHINES----------------------------------------------------------------

# --------------------------------------------RUN FROM BELOW FOR SVM USING DF1------------------------------------------
set.seed(324)
svm_df1 <- df1
svm_df1_test <- df1_test
svm_df1$Class <- factor(svm_df1$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df1_test$Class <- factor(svm_df1_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model1_tuned <- train(
  Class ~ .,  
  data = svm_df1,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),  # Standardize features for SVM
  metric = "Kappa"
)
print(svm_model1_tuned$bestTune)
pred_prob <- predict(svm_model1_tuned, svm_df1_test, type = "prob")
roc_curve <- roc(response = svm_df1_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df1_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df1 using SVM:")
print(cm)
print("Performance for df1 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR SVM USING DF2------------------------------------------
set.seed(324)
svm_df2 <- df2
svm_df2_test <- df2_test
svm_df2$Class <- factor(svm_df2$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df2_test$Class <- factor(svm_df2_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.0001,0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model2_tuned <- train(
  Class ~ .,  
  data = svm_df2,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  metric="Kappa"
)
print(svm_model2_tuned$bestTune)
pred_prob <- predict(svm_model2_tuned, svm_df2_test, type = "prob")
roc_curve <- roc(response = svm_df2_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df2_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df2 using SVM:")
print(cm)
print("Performance for df2 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR SVM USING DF3------------------------------------------
set.seed(324)
svm_df3 <- df3
svm_df3_test <- df3_test
svm_df3$Class <- factor(svm_df3$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df3_test$Class <- factor(svm_df3_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model3_tuned <- train(
  Class ~ .,  
  data = svm_df3,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  metric="Kappa"
)
print(svm_model3_tuned$bestTune)
pred_prob <- predict(svm_model3_tuned, svm_df3_test, type = "prob")
roc_curve <- roc(response = svm_df3_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df3_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df3 using SVM:")
print(cm)
print("Performance for df3 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)


# --------------------------------------------RUN FROM BELOW FOR SVM USING DF4------------------------------------------
set.seed(324)
svm_df4 <- df4
svm_df4_test <- df4_test
svm_df4$Class <- factor(svm_df4$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df4_test$Class <- factor(svm_df4_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model4_tuned <- train(
  Class ~ .,  
  data = svm_df4,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  metric="Kappa"
)
print(svm_model4_tuned$bestTune)
pred_prob <- predict(svm_model4_tuned, svm_df4_test, type = "prob")
roc_curve <- roc(response = svm_df4_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df4_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df4 using SVM:")
print(cm)
print("Performance for df4 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR SVM USING DF5------------------------------------------
set.seed(324)
svm_df5 <- df5
svm_df5_test <- df5_test
svm_df5$Class <- factor(svm_df5$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df5_test$Class <- factor(svm_df5_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model5_tuned <- train(
  Class ~ .,  
  data = svm_df5,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  metric="Kappa"
)
print(svm_model5_tuned$bestTune)
pred_prob <- predict(svm_model5_tuned, svm_df5_test, type = "prob")
roc_curve <- roc(response = svm_df5_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df5_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df5 using SVM:")
print(cm)
print("Performance for df5 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)

# --------------------------------------------RUN FROM BELOW FOR SVM USING DF6------------------------------------------
set.seed(324)
svm_df6 <- df6
svm_df6_test <- df6_test
svm_df6$Class <- factor(svm_df6$Class, levels = c(0, 1), labels = c("Class0", "Class1"))
svm_df6_test$Class <- factor(svm_df6_test$Class, levels = c(0, 1), labels = c("Class0", "Class1"))

control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, classProbs = TRUE)

tune_grid <- expand.grid(
  sigma = c(0.001, 0.01, 0.1),
  C = c(0.01, 0.1, 1, 10)
)
svm_model6_tuned <- train(
  Class ~ .,  
  data = svm_df6,  
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  preProcess = c("center", "scale")
)
print(svm_model6_tuned$bestTune)
pred_prob <- predict(svm_model6_tuned, svm_df6_test, type = "prob")
roc_curve <- roc(response = svm_df6_test$Class, predictor = pred_prob[, "Class1"])
auc_value <- roc_curve$auc
pred <- ifelse(pred_prob[, "Class1"] > 0.5, "Class1", "Class0")
cm <- table(Actual = svm_df6_test$Class, Predicted = pred)
rownames(cm) <- c("0", "1")
colnames(cm) <- c("0", "1")
print("Confusion Matrix for df6 using SVM:")
print(cm)
print("Performance for df6 using SVM:")
metrics_table <- calculate_metrics(cm, auc_value)
print(metrics_table)



