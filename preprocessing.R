df <- read.csv('project_data.csv')
dim(df)
head(df)
sapply(df, class)

df <- df[, !(names(df) %in% c("SERIALNO"))]

unique(df$PERNP)

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

impute_map <- list(
  COW = 0, POVPIP = -1, JWRIP = 0, ENG = 0, PERNP = 0,
  JWMNP = 0, FER = 0, SCIENGRLP = 0, WKHP = 0, GCL = 0, SCIENGP = 0, MARHYP = -1,
  RC = -1, WKWN = 0, POWSP = 0, YOEP = -1, MARHD = 0, POWPUMA = 0,
  MARHM = 0, PAOC = 0, MARHT = 0, OC = -1, MARHW = 0, MIL = 0,
  LANP = 0, JWDP = 0,
  JWAP = 0, INDP = 0, FOD1P = 0,
  ESR = 0, NWAB = 0, NWAV = 0, DRIVESP = 0, NWLA = 0, DECADE = 0,
  NWLK = 0, OCCP = 0, NWRE = 0, WRK = 0, JWTRNS = 0
)

# Loop through each column and impute the specified value
for (col in names(impute_map)) {
  if (col %in% names(df)) {
    df[[col]][is.na(df[[col]])] <- impute_map[[col]]
  }
}

sapply(df, function(x) sum(is.na(x)))

# DATA TYPES CORRECTION
# Check the current data type of each variable
sapply(df, class)
drop_columns
cols_to_factor <- c("RT", "DIVISION", "PUMA", "REGION",
                   "STATE", "ADJINC", "CIT", "COW", "ENG",
                   "FER", "GCL", "HIMRKS", 
                   "HINS1", "HINS2", "HINS3", "HINS4",
                   "HINS5", "HINS6", "HINS7", "JWTRNS", 
                   "LANX", "MAR", "MARHD", "MARHM", "MARHT", 
                   "MARHW", "MIG", "MIL", "NWAB", 
                   "NWAV", "NWLA", "NWLK", "NWRE", "SCH",
                   "SCHL", "SEX", "WRK", "ANC", "ANC1P", "ANC2P",
                   "DECADE", "DRIVESP", "ESR", "FOD1P",
                   "HICOV", "HISP", "INDP", "JWAP", "JWDP", "LANP",
                   "MSP", "NATIVITY", "OC", "OCCP", "PAOC",
                   "POBP", "POWPUMA", "POWSP", "PRIVCOV", "PUBCOV", "QTRBIR",
                   "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACASN", "RACBLK",
                   "RACNH", "RACNUM", "RACPI", "RACSOR", "RACWHT", "RC", "SCIENGP",
                   "SCIENGRLP", "WAOB", "Class")

df[cols_to_factor] <- lapply(df[cols_to_factor], as.factor)

# Convert integer variables in int_to_numeric to numeric
int_to_numeric <- sapply(df, is.integer)
df[int_to_numeric] <- lapply(df[int_to_numeric], as.numeric)


sapply(df, class)



# Check for near zero variance variables
install.packages("caret")
library(caret)
low_var_col <- nearZeroVar(df, names = TRUE)
low_var_col
df <- df[, !(names(df) %in% low_var_col)]
dim(df)


# OUTLIERS HANDLING
# Visualize numeric data with boxplots
install.packages("reshape2")
library(reshape2)
library(ggplot2)
numeric_columns <- sapply(df, is.numeric)
df_long <- melt(df[, numeric_columns])

ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Variables", y = "Values", title = "Boxplots of Numeric Variables") +
  theme(
    axis.text.x = element_blank(),  # Removes x-axis labels
    axis.ticks.x = element_blank()  # Removes x-axis ticks
  )


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

outlier_cols_handle <- c("WAGP", "PERNP", "PINCP")

# Handle the outliers based on IQR (assign them to lower or upper bounds)
for (col in outlier_cols_handle) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  LB <- Q1 - 1.5 * IQR
  UB <- Q3 + 1.5 * IQR
  
  df[[col]][df[[col]] < LB] <- LB
  df[[col]][df[[col]] > UB] <- UB
}

df_long <- melt(df[, numeric_columns])

ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Variables", y = "Values", title = "Boxplots of Numeric Variables") +
  theme(
    axis.text.x = element_blank(),  # Removes x-axis labels
    axis.ticks.x = element_blank()  # Removes x-axis ticks
  )

sapply(df, class)

#BINNING
#SPORDER - keep as it is
#PWGTP - apparently it is not physical weight
#JWRIP - no need for binning, only 11 values

h <- hist(df$JWMNP, breaks=5, plot=FALSE)
h$breaks

hist(df$JWMNP)
unique(df$JWMNP)
df$JWMNP

df$JWMNP <- ifelse(df$JWMNP == 0, 0,
                           ifelse(df$JWMNP >= 1 & df$JWMNP <= 10, 1,
                           ifelse(df$JWMNP > 10 & df$JWMNP <= 30, 2,
                           ifelse(df$JWMNP > 30 & df$JWMNP <= 60, 3,
                           ifelse(df$JWMNP > 60 & df$JWMNP <= 100, 4,
                           ifelse(df$JWMNP > 100 & df$JWMNP <= 200, 5,
                           ifelse(df$JWMNP == 888, 6, NA)))))))

hist(df$JWRIP)
df$JWRIP

hist(df$WAGP)
unique(df$WAGP)

df$WAGP <- ifelse(df$WAGP == 0, 0,
           ifelse(df$WAGP <= 20000, 1,
           ifelse(df$WAGP <= 50000, 2,
           ifelse(df$WAGP <= 100000, 3,
           ifelse(df$WAGP <= 150000, 4, 5)))))
df$WAGP

#For WKHP
df$WKHP
df$WKHP <- ifelse(df$WKHP == 0, 0,  # NA values already converted to 0
                  ifelse(df$WKHP >= 1 & df$WKHP <= 20, 1,  # Between 1 and 20 hours
                         ifelse(df$WKHP > 20 & df$WKHP <= 40, 2,  # Between 20 and 40 hours
                                ifelse(df$WKHP > 40 & df$WKHP <= 60, 3,  # Between 40 and 60 hours
                                       ifelse(df$WKHP > 60 & df$WKHP <= 98, 4,  # Between 60 and 98 hours
                                              ifelse(df$WKHP >= 99, 5, NA))))))  # 99 or more hours


hist(df$WKHP)
df$WKHP
test <- df
#For WKWN
df$WKWN
hist(df$WKWN,breaks=5)
unique(df$WKWN)
df$WKWN <- ifelse(df$WKWN == 0, 0,  # NA values already converted to 0
                  ifelse(df$WKWN >= 1 & df$WKWN <= 13, 1,  # 1-13 weeks (1st quarter)
                         ifelse(df$WKWN > 13 & df$WKWN <= 26, 2,  # 14-26 weeks (2nd quarter)
                                ifelse(df$WKWN > 26 & df$WKWN <= 39, 3,  # 27-39 weeks (3rd quarter)
                                       ifelse(df$WKWN > 39 & df$WKWN <= 51, 4,  # 40-51 weeks (almost full year)
                                              ifelse(df$WKWN == 52, 5, NA))))))  # 52 weeks (full year)
df$WKWN
hist(df$WKWN)

#For PERNP
hist(df$PERNP)

hist(df$PERNP, breaks =5)
unique(df$PERNP)
df$PERNP<- ifelse(df$PERNP == 0,0,  # No earnings
                       ifelse(df$PERNP == -10000, 1,  # Significant loss
                              ifelse(df$PERNP >= -9999 & df$PERNP <= -1, 2,  # Small losses
                                     ifelse(df$PERNP >= 1 & df$PERNP <= 10000, 3,  # Low earners
                                            ifelse(df$PERNP > 10000 & df$PERNP <= 50000, 4,  # Middle earners
                                                   ifelse(df$PERNP > 50000 & df$PERNP <= 200000, 5,  # High earners
                                                          ifelse(df$PERNP > 200000, 6,  # Very high earners
                                                                 NA)))))))  
df$PERNP
hist(df$PERNP)

#For PINCP
hist(test$PINCP)
unique(test$PINCP)

df$PINCP <- ifelse(df$PINCP == 0, 0,  # No income
                       ifelse(df$PINCP == -19998, 1,  # Loss of $19,998 or more
                              ifelse(df$PINCP >= -19997 & df$PINCP <= -1, 2,  # Losses between $1 and $19,997
                                     ifelse(df$PINCP >= 1 & df$PINCP <= 10000, 3,  # Low income
                                            ifelse(df$PINCP > 10000 & df$PINCP <= 50000, 4,  # Lower middle income
                                                   ifelse(df$PINCP > 50000 & df$PINCP <= 100000, 5,  # Middle to upper middle income
                                                          ifelse(df$PINCP > 100000 & df$PINCP <= 4209995, 6,  # High income
                                                                  NA)))))))
df$PINCP
hist(df$PINCP)

#POVPIP
df$POVPIP
hist(df$POVPIP)
unique(df$POVPIP)
df$POVPIP <- ifelse(df$POVPIP == -1, 0,  # NA (Under 15 years)
                          ifelse(df$POVPIP >= 0 & df$POVPIP <= 99, 1,  # 0 to 99% of the poverty level
                                 ifelse(df$POVPIP >= 100 & df$POVPIP <= 199, 2,  # 100 to 199% of the poverty level
                                        ifelse(df$POVPIP >= 200 & df$POVPIP <= 299, 3,  # 200 to 299% of the poverty level
                                               ifelse(df$POVPIP >= 300 & df$POVPIP <= 399, 4,  # 300 to 399% of the poverty level
                                                      ifelse(df$POVPIP >= 400 & df$POVPIP <= 500, 5,  # 400 to 500% of the poverty level
                                                             ifelse(df$POVPIP == 501, 6, NA)))))))  # 501% or more of the poverty level
df$POVPIP
hist(df$POVPIP)
