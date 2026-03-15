library(mlbench)
library(randomForest)
library(adabag)
library(caret)

data(Soybean)
df <- na.omit(Soybean)
df$Class <- droplevels(df$Class)

idx <- sample(1:nrow(df), 0.7 * nrow(df))
train <- df[idx, ]
test <- df[-idx, ]

# BAGGING
bag_mod <- randomForest(Class ~ ., data=train, mtry=ncol(train)-1)
bag_pred <- predict(bag_mod, test)
print(confusionMatrix(bag_pred, test$Class))

# BOOSTING
bst_mod <- boosting(Class ~ ., data=train, mfinal=10)
bst_pred <- predict(bst_mod, test)
print(confusionMatrix(as.factor(bst_pred$class), test$Class))