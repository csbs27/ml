library(mlbench)
library(caret)
library(nnet)  

data(Soybean)
Soybean <- na.omit(Soybean)  

set.seed(123)
train_index <- createDataPartition(Soybean$Class, p = 0.7, list = FALSE)
train_data <- Soybean[train_index, ]
test_data  <- Soybean[-train_index, ]

train_data$Class <- droplevels(train_data$Class)
test_data$Class  <- droplevels(test_data$Class)

set.seed(123)
mlp_model <- train(
  Class ~ .,
  data = train_data,
  method = "nnet",            
  trControl = trainControl(method = "cv", number = 10),  
  tuneLength = 5,            
  trace = FALSE              
)

print(mlp_model)

mlp_predictions <- predict(mlp_model, newdata = test_data)

confusionMatrix(
  mlp_predictions,
  test_data$Class
)