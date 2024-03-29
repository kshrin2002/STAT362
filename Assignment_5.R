library(nnet)
library(magrittr)
library(tidyverse)
library(GGally)
# Q1
wine_df <- read.csv("C:/Users/jayan/Downloads/winequality-white.csv",sep=";")

wine_df <- wine_df %>%
  mutate(quality_group = case_when(
    quality <= 5 ~ "Below",
    quality == 6 ~ "Average",
    quality > 6 ~ "Good"
  ))

# Q2
ggpairs(data = wine_df, aes(color = factor(wine_df$quality_group, levels = c("Below", "Average", "Good")), alpha = 0.8), columns = c(4, 8, 11))

# Q3
# indices corresponding to training data
set.seed(1)
random_index <- sample(1:nrow(wine_df), 4000, replace = FALSE)

train_data  <- wine_df[random_index, ]
test_data <- wine_df[-random_index, ]

model <- multinom(quality_group ~ . - quality, data = train_data)

coefficients <- coef(model)
coefficients



# q4a
train_predictions <- predict(model, train_data)
train_real <- train_data$quality_group
train_confusion_matrix <- table(Predicted = train_predictions, Actual = train_real)
train_confusion_matrix
training_accuracy <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
print(paste("Training Accuracy:", training_accuracy))

# q4b
test_predictions <- predict(model, test_data)
test_real <- test_data$quality_group
test_confusion_matrix <- table(Predicted = test_predictions, Actual = test_real)
print(test_confusion_matrix)
testing_accuracy <- sum(diag(test_confusion_matrix)) / sum(test_confusion_matrix)
print(paste("Testing Accuracy:", testing_accuracy))

# Q5
set.seed(1)
n <- 1000
X <- rnorm(n, 0.01, 0.05)
Y <- 0.5 * X + rnorm(n, 0, 0.05)
data <- cbind(X, Y)

ratio <- function(data,indices) {
  sample<- data[indices, ]
  sX <- sd(sample[, 1])
  sY <- sd(sample[, 2])
  return(sX / sY)
}

bootstraps_val <- 1000

bootstrap_estimates <- replicate(bootstraps_val, ratio(data, sample(1:n, n, replace = TRUE)))
standard_error_estimate <- sd(bootstrap_estimates)
standard_error_estimate
