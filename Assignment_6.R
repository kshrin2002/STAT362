library(dplyr)
library(gmodels)
library(ISLR2)
library(randomForest)
library(tidyverse)
library(tree)
library(mlbench)
library(FNN)

# Q1
wine_df <- read.csv('C:/Users/jayan/Downloads/winequality-white.csv',sep = ";")
wine_df <- filter(wine_df, quality > 3, quality < 9) # remove data with quality 3 and 9
set.seed(1)
random_index <- sample(1:nrow(wine_df), 4000, replace = FALSE) # indices for training data
wine_df$quality <- factor(wine_df$quality)

training_wine <- wine_df[random_index,]
testing_wine <- wine_df[-random_index,]

random_forest <- randomForest(quality ~., data = training_wine,mtry = (ncol(training_wine) - 1)/ 3, ntree = 1000, importance = TRUE)
random_forest_prediction <- predict(random_forest,testing_wine)

testing_value <- wine_df[-random_index, ]$quality
confusion_matrix <- table(random_forest_prediction,testing_value)
CrossTable(random_forest_prediction,testing_wine$quality,prop.chisq = FALSE)

mean(random_forest_prediction == testing_value)
# This prediction is more accurate than the example in Chapter 12

# Q2
concrete <- read.csv("C:/Users/jayan/Downloads/concrete.csv") # write your own path
names(concrete)[1] <- "cement"
set.seed(2)
index <- sample(nrow(concrete), 700) # indices corresponding to the training data
concrete_train <- concrete[index, ]
concrete_test <- concrete[-index, ]

# Remove missing values (train)
training_concrete <- na.omit(concrete_train)

# Remove missing values (test)
testing_concrete <- na.omit(concrete_test)

regression_tree <- tree(strength ~. , concrete_train)
plot(regression_tree)
text(regression_tree)
mean((concrete_test$strength - predict(regression_tree, concrete_test))^2)

# Q3 
linreg_model <- lm(strength ~. , data = concrete_train)
predictions <- predict(linreg_model, datanew = concrete_test)
mean((concrete_test$strength - predictions)^2)

# Q4a
random_forest_new <- randomForest(strength ~., data = concrete_train,
                                  mtry=(ncol(concrete_train)-1)/ 3, ntree=1000,importance = TRUE)
random_forest_prediction_new <- predict(random_forest_new,concrete_test)

mean((concrete_test$strength-random_forest_prediction_new)^2)

# Q4b
importance(random_forest_new)
varImpPlot(random_forest_new)

# Age is the "most important" variable

# Q5
scaled <- function(train,test){
  train_knn <- train
  test_knn <- test
  # Using min max scale
  train_minimum <- apply(train,2,min)
  train_maximum <- apply(train,2,max)
  for (i in 1:ncol(train)) {
    train_knn[,i] <- (train_knn[,i] - train_minimum[i])/(train_maximum[i] - train_minimum[i])
    
    # normalize
    test_knn[,i] <- (test_knn[,i] - train_minimum[i])/(train_maximum[i] - train_minimum[i])
  }
  return(list(train=train_knn,test=test_knn))
}

train_predictions <- concrete_train[,-which(names(concrete_train) == 'strength')]
training_rep <- concrete_train$strength

test_predictions <- concrete_test[,-which(names(concrete_test) == 'strength')]

data_scaled <- scaled(train_predictions,test_predictions)

train_predictions_scaled<- data_scaled$train
test_predictions_scaled <- data_scaled$test

knn_scaled <- knn.reg(train=train_predictions_scaled,test=test_predictions_scaled,y = training_rep,k=5)
predictions_total_scaled <- knn_scaled$pred

mean((concrete_test$strength - predictions_total_scaled)^2)

# Q6
mse_vals <- c(
  mean((concrete_test$strength - predict(regression_tree,concrete_test))^2),
  mean((concrete_test$strength - predictions)^2),
  mean((concrete_test$strength - random_forest_prediction_new)^2),
  mean((concrete_test$strength - predictions_total_scaled)^2)
)

method_types <- c("Regression Tree","Linear Regression","Random Forest","KNN")
result <- data.frame(Method = method_types,MSE = mse_vals)

# Order of effectiveness: Random forest (most effective due to the low MSE), KNN, regression tree and least effective is linear regression 

