#visualizations before data cleaning
# Age Distribution (Histogram)
hist(df$AGEP, main = "Age Distribution", xlab = "Age", col = "skyblue")

# Race Distribution (Bar Plot)
barplot(table(df$RAC1P), main = "Race Distribution", xlab = "Race", ylab = "Count", col = "orange")

# Sex Distribution (Bar Plot)
barplot(table(df$SEX), main = "Sex Distribution", xlab = "Sex", ylab = "Count", col = "pink")

# Income Distribution (Histogram)
hist(df$PINCP, main = "Income Distribution", xlab = "Income", col = "lightgreen", breaks = 50)

# Income by Education (Boxplot)
boxplot(PINCP ~ SCHL, data = df, main = "Income by Education Level", xlab = "Education Level", ylab = "Income", col = "lightblue")

# Employment Status (Bar Plot)
barplot(table(df$ESR), main = "Employment Status", xlab = "Status", ylab = "Count", col = "lightyellow")

# Occupation Distribution (Bar Plot)
barplot(table(df$OCCP), main = "Occupation Distribution", xlab = "Occupation Code", ylab = "Count", col = "lightcoral")

# Population by PUMA (Bar Plot)
barplot(table(df$PUMA), main = "Population by PUMA", xlab = "PUMA", ylab = "Count", col = "lightblue")

# Migration Patterns (Bar Plot)
barplot(table(df$MIG), main = "Migration Status", xlab = "Migration Status", ylab = "Count", col = "lightgreen")

# Education Level (Bar Plot)
barplot(table(df$SCHL), main = "Education Level", xlab = "Education Level", ylab = "Count", col = "lightpink")

# Work Hours (Histogram)
hist(df$WKHP, main = "Usual Hours Worked Per Week", xlab = "Hours", col = "lightblue")

# Correlation Matrix (Heatmap)
install.packages("corrplot")
library(corrplot)
numeric_vars <- df[, sapply(df, is.numeric)]  # Select numeric columns
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper")

# Missing Data Pattern
install.packages("naniar")
library(naniar)
gg_miss_upset(df)  # Visualize missing data patterns

# Check for duplicate rows
sum(duplicated(df))

