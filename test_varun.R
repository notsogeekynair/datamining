clean_df <- read.csv("preprocessed_data.csv")
table(clean_df$Class)
# Load required libraries
library(caret)
library(ROSE)

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and testing (30%) sets
split_index <- createDataPartition(clean_df$Class, p = 0.8, list = FALSE)
train_data <- clean_df[split_index, ]
test_data <- clean_df[-split_index, ]

# Check class distribution in training set
print("Original training data class distribution:")
table(train_data$Class)

# Load required libraries
library(ROSE)
set.seed(123)

# Keep all original majority class samples
majority_samples <- train_data[train_data$Class == "No", ]  # 3201 "No" samples

# Generate synthetic minority samples to match majority class count
# SMOTE portion
synthetic_result <- ROSE(Class ~ ., data = train_data, 
                         N = 2 * nrow(train_data),  # Generate enough samples 
                         p = 0.5)$data

# Extract only the "Yes" samples from the SMOTE result
synthetic_minority <- synthetic_result[synthetic_result$Class == "Yes", ]

# Take exactly 3201 minority samples to match majority count
synthetic_minority <- synthetic_minority[1:3201, ]

# Combine with original majority samples to create balanced training set
train_balanced <- rbind(majority_samples, synthetic_minority)

# Verify the class distribution
table(train_balanced$Class)
