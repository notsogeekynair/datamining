df <- read.csv('project_data.csv')
dim(df)
head(df)

# convert class to factor
df$Class <- as.factor(df$Class)
sapply(df, class)

int_to_char <- c("DIVISION", "PUMA", "REGION",
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
df[int_to_char] <- lapply(df[int_to_char], as.character)
sapply(df, class)




# Identify numeric and categorical columns
numeric_columns <- sapply(df, is.numeric)
categorical_columns <- sapply(df, is.character)

# 1️⃣ For Numeric Columns: Remove columns with near-zero variance
low_variance_numeric <- names(df)[numeric_columns][sapply(df[numeric_columns], function(col) var(col, na.rm = TRUE) == 0)]

# 2️⃣ For Categorical Columns: Remove columns with only one unique value
low_variability_categorical <- names(df)[categorical_columns][sapply(df[categorical_columns], function(col) length(unique(col)) == 1)]

# Combine low variability columns
low_variability_columns <- c(low_variance_numeric, low_variability_categorical)

# Remove these columns from df
df <- df[, !names(df) %in% low_variability_columns]

# ✅ Check remaining columns
dim(df)
print(low_variability_columns)




sapply(df, function(x) sum(is.na(x)))

missing_percentage <- colMeans(is.na(df)) * 100
missing_percentage
columns_to_remove <- names(missing_percentage[missing_percentage > 80])
columns_to_remove
df <- df[, !names(df) %in% columns_to_remove]
dim(df)
sapply(df, function(x) sum(is.na(x)))






# Function to calculate the mode
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Exclude NA values
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing values with mode for character columns
char_columns <- sapply(df, is.character)

df[char_columns] <- lapply(df[char_columns], function(col) {
  if (any(is.na(col))) {
    mode_value <- get_mode(col)
    col[is.na(col)] <- mode_value
  }
  return(col)
})

# Check if missing values are handled
sapply(df, function(x) sum(is.na(x)))

sapply(df, class)





# Identify integer columns
int_columns <- sapply(df, is.integer)

# Replace missing values with 9999 in specified columns
cols_to_replace <- c("MARHYP", "YOEP")

# Apply replacement
df[cols_to_replace] <- lapply(df[cols_to_replace], function(col) {
  col[is.na(col)] <- 2024
  return(col)
})

# Replace missing values with 9999 in specified columns
cols_to_replace <- c("JWMNP")

# Apply replacement
df[cols_to_replace] <- lapply(df[cols_to_replace], function(col) {
  col[is.na(col)] <- 201
  return(col)
})

# Verify if missing values are handled
sapply(df[cols_to_replace], function(x) sum(is.na(x)))

# View updated data
head(df[cols_to_replace])


# Impute missing values with median for integer columns
df[int_columns] <- lapply(df[int_columns], function(col) {
  if (any(is.na(col))) {
    median_value <- median(col, na.rm = TRUE)
    col[is.na(col)] <- median_value
  }
  return(col)
})

# Check if missing values are handled
sapply(df, function(x) sum(is.na(x)))




install.packages("gridExtra")

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Function to create histograms for numeric columns
create_histogram <- function(df, col_name) {
  ggplot(df, aes_string(x = col_name)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency")
}

# Function to create bar plots for character columns
create_barplot <- function(df, col_name) {
 ggplot(df, aes_string(x = col_name)) +
  geom_bar(fill = "coral", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Bar Plot of", col_name), x = col_name, y = "Count")
}

# Identify numeric and character columns
numeric_columns <- sapply(df, is.numeric)
character_columns <- sapply(df, is.character)

# Create a list of plots for both numeric and character columns
plots <- list()

# Add numeric histograms
for (col_name in names(df)[numeric_columns]) {
  plots[[length(plots) + 1]] <- create_histogram(df, col_name)
}

# Add bar plots for character columns
for (col_name in names(df)[character_columns]) {
  plots[[length(plots) + 1]] <- create_barplot(df, col_name)
}

# Arrange plots in a grid (adjust ncol and nrow as needed)
grid.arrange(grobs = plots, ncol = 3)


create_histogram(df, "JWMNP")
