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

# Set seed for reproducibility
set.seed(123)

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