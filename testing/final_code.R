df <- read.csv('project_data.csv')
dim(df)
head(df)

# DATA TYPES CORRECTION
# Check the current data type of each variable
sapply(df, class)

int_to_factor <- c("DIVISION", "PUMA", "REGION",
                 "STATE", "ADJINC", "CIT", "COW", "ENG",
                 "FER", "GCL", "GCM", "GCR", "HIMRKS", 
                 "HINS1", "HINS2", "HINS3", "HINS4",
                 "HINS5", "HINS6", "HINS7", "JWTRNS", 
                 "LANX", "MAR", "MARHD", "MARHM", "MARHT", 
                 "MARHW", "MIG", "MIL", "MLPA", "MLPB", "MLPCD", 
                 "MLPE", "MLPFG", "MLPH", "MLPIK", "MLPJ", "NWAB", 
                 "NWAV", "NWLA", "NWLK", "NWRE", "SCH", "SCHG",
                 "SCHL", "SEX", "WRK", "ANC", "ANC1P", "ANC2P",
                 "DECADE", "DRIVESP", "ESP", "ESR", "FOD1P", "FOD2P",
                 "HICOV", "HISP", "INDP", "JWAP", "JWDP", "LANP", "MIGPUMA",
                 "MIGSP", "MSP", "NATIVITY", "NOP", "OC", "OCCP", "PAOC",
                 "POBP", "POWPUMA", "POWSP", "PRIVCOV", "PUBCOV", "QTRBIR",
                 "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACASN", "RACBLK",
                 "RACNH", "RACNUM", "RACPI", "RACSOR", "RACWHT", "RC", "SCIENGP",
                 "SCIENGRLP", "SFN", "SFR", "VPS", "WAOB")
df[int_to_factor] <- lapply(df[int_to_factor], as.factor)

# Convert integer variables in int_to_numeric to numeric
int_to_numeric <- sapply(df, is.integer)
df[int_to_numeric] <- lapply(df[int_to_numeric], as.numeric)

sapply(df, class)

# DATA CLEANING
# Check for duplicate rows
cat("Number of duplicate rows:", nrow(df[duplicated(df), ]))

# Check the number of missing values in each column
sapply(df, function(x) sum(is.na(x)))

# Remove the columns with more than 80% of missing values
drop_columns <- names(which(colMeans(is.na(df)) > 0.8))
drop_columns
df <- df[, !names(df) %in% drop_columns]
dim(df)

sapply(df, function(x) sum(is.na(x)))

# Impute missing values in factor columns with mode
calculate_mode <- function(x) {
  unique_val <- unique(x[!is.na(x)])
  unique_val[which.max(tabulate(match(x, unique_val)))]
}

factor_columns <- sapply(df, is.factor)

df[factor_columns] <- lapply(df[factor_columns], function(col) {
  if (any(is.na(col))) {
    mode_value <- calculate_mode(col)
    col[is.na(col)] <- mode_value
  }
  return(col)
})

sapply(df, function(x) sum(is.na(x)))

# Impute missing values in numeric columns with median
calculate_median <- function(x) {
  median(x, na.rm = TRUE)
}

numeric_columns <- sapply(df, is.numeric)

df[numeric_columns] <- lapply(df[numeric_columns], function(col) {
  if (any(is.na(col))) {
    median_value <- calculate_median(col)
    col[is.na(col)] <- median_value
  }
  return(col)
})

sapply(df, function(x) sum(is.na(x)))

# OUTLIERS HANDLING
# Visualize numeric data with boxplots
install.packages("reshape2")
library(reshape2)
library(ggplot2)
df_long <- melt(df[, numeric_columns])

ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 1) +
  scale_y_log10() +
  theme_minimal() +
  labs(x = "Variables", y = "Values", title = "Boxplots of Numeric Variables")


# Count the outliers in each column
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

# Handle the outliers based on IQR (assign them to lower or upper bounds)
for (col in names(df)[numeric_columns]) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  LB <- Q1 - 1.5 * IQR
  UB <- Q3 + 1.5 * IQR
  
  df[[col]][df[[col]] < LB] <- LB
  df[[col]][df[[col]] > UB] <- UB
}

# Count the outliers after handling
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

# Check for near zero variance variables
install.packages("caret")
library(caret)
low_var_col <- nearZeroVar(df, names = TRUE)
df <- df[, !(names(df) %in% low_var_col)]
dim(df)