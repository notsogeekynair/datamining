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