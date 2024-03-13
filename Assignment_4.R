
# Q1a
library(ggplot2)
library(class)
library(ISLR2)
x <- c(171.6, 191.8, 178.3, 184.9, 189.1)
t.test(x,alternative = "less", mu = 185)

# Q1b
x <- c(142600, 167800, 136500, 108300, 126400, 133700, 162000, 149400)
t.test(x)$conf.int

# Q1c
prop.test(x = 35, n = 400)

# Q1d

favor_a <- c(232, 260, 197) 
total_shoppers <- c(400, 500, 400) 
prop.test(favor_a, total_shoppers)

# Q2

iris_train <- read.csv("C:/Users/jayan/Downloads/iris_train.csv")
iris_test <- read.csv("C:/Users/jayan/Downloads/iris_test.csv")

normalize_data <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
    (x - min(x)) / (max(x) - min(x))
  })
  return(data)
}

iris_train_normalize <- normalize_data(iris_train)
iris_test_normalize <- normalize_data(iris_test)

# Q3a
iris_training_labs <- iris_train$Species
iris_train_new <- iris_train_normalize[, -ncol(iris_train_normalize)]
iris_test_new <- iris_test_normalize[, -ncol(iris_test_normalize)]
iris_knn_model <- knn(train = iris_train_new, test = iris_test_new,
                      cl = iris_training_labs, k = 3)

# Q3b
sum(diag(table(iris_knn_model,iris_test$Species))) / nrow(iris_test_new)

# Q4
?knn
iris_knn_model <- knn(train = iris_train_new, test = iris_test_new,
                      cl = iris_training_labs, k = 3, prob = TRUE)

# Q5

Hitters <- na.omit(Hitters)
?Hitters

# League, division and NewLeague are the categorical variables

# Q6a

ggplot(Hitters, aes(x = Years, y = Salary)) +
  geom_point() +
  labs(x = "Years of Experience",
       y = "Salary",
       title = "Relationship between Salaries and Years of Experience") +
  theme_minimal()

# Q6b

ggplot(Hitters, aes(x = Years, y = Salary, color = Division)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Years of Experience", y = "Salary", title = "Relationship between Salaries and Years of Experience",
       color = "Division")

# Q7a
ggplot(Hitters, aes(x = Salary)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  labs(x = "Salary",
       y = "Density",
       title = "Distribution of Player Salaries in the Hitters Dataset") +
  theme_minimal()

# There is a general down-ward trend of density, as the salary gets higher the density decreases. 

# Q7b

ggplot(Hitters, aes(x = Salary)) +
  geom_density(fill = "red") +
  theme_minimal() +
  labs(x = "Salary", y = "Density", title = "Kernel Density Estimate")

# Q7c
# The histogram shows the lowest pay has the highest number of players
# The density graph shows the most players are slightly above the lowest pay grade

# Q8a
data = Hitters
model <- lm(Salary ~ CHits, data)
summary(model)

# Q8b
# The coefficient is 0.38202. For every additional hit, the player's salary increases by 0.38202 units

# Q9a
CHits_quan <- quantile(Hitters$CHits, c(0.2, 0.4, 0.6, 0.8))
cat_CHits <- rep(0, nrow(Hitters))
cat_CHits[Hitters$CHits <= CHits_quan[1]] <- 1
cat_CHits[Hitters$CHits > CHits_quan[1] & Hitters$CHits <= CHits_quan[2]] <- 2
cat_CHits[Hitters$CHits > CHits_quan[2] & Hitters$CHits <= CHits_quan[3]] <- 3
cat_CHits[Hitters$CHits > CHits_quan[3] & Hitters$CHits <= CHits_quan[4]] <- 4
cat_CHits[Hitters$CHits > CHits_quan[4]] <- 5
cat_CHits <- factor(cat_CHits)

new_cat_CHits <- lm(Salary ~ cat_CHits, data)
summary(new_cat_CHits)

# Q9b
# The coefficients indicate the relative difference in salary compared to the baseline group (categories 2 to 5), with 
# categories 3, 4, and 5 showing statistically significant increases in salary.


# Q10a
set.seed(1)
index <- sample(nrow(Hitters), nrow(Hitters) * 0.5)
train <- Hitters[index, ]
test <- Hitters[-index, ]

# Q10a
fit <- lm(Salary ~. , data = train)
summary(fit)

# Q10b
# CAtBat: p-value = 0.0116
# CHits: p-value = 0.0073
# DivisionW: p-value = 0.0112
# Assists: p-value = 0.0279

# Q10c
# The estimated regression coefficient of CHits is 3.1632., this means that each additional career hit (CHits) 
# increases by 3.1632 units 

# Q10d
res <- fit$res
mse <- mean(res^2)
print(mse)

# Q10e
predicted_salaries <- predict(fit, newdata = test)
res_test <- test$Salary - predicted_salaries
mse_test <- mean(res_test^2)
print(mse_test)

