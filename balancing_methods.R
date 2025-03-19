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