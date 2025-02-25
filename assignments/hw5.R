
library(rsample)
library(caret)
library(dplyr)

df <- read.csv('heart_disease(2).csv')

df$class <- as.factor(df$class)
str(df)
set.seed(154)
split <- initial_split(df, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)


tunegrid <- expand.grid(
  layer1 = c(5, 10, 15),  
  layer2 = c(0,0,0),
  layer3 = c(0,0,0),        
  decay = c(0.1, 0.01, 0.001)  
)
tunegrid

set.seed(154)
model <- train(
  class ~ .,  
  data = train,  
  method = "mlpWeightDecayML",
  tuneGrid = tunegrid,
  trControl = trainControl(method = "cv", number = 10),
  preProc = c("center", "scale") #scaling numerical columns
)


print(model$bestTune)
predictions <- predict(model, newdata = test)
performance_measures <- confusionMatrix(predictions, test$class)
print(performance_measures)
plot(model)

