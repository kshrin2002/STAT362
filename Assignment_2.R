library(nycflights13)
library(tidyverse)

# Q1
A2_q1 <- function(input_matrix) {
  input_matrix[input_matrix < 0] <- 0
  return(input_matrix)
}

# Test
A <- matrix(c(-2, 1, 1, 3), 2, 2)

result <- A2_q1(A)
result

# Q2
A2_q2 <- function(square_matrix){
  diagonal_sum <- sum(diag(square_matrix))
  return(diagonal_sum)
}

# Test
A <- matrix(c(-2, 1, 1, 10), 2, 2)
result <- A2_q2(A)
result

# Q3
A2_q3 <- function(df, col){
  missing_vals <- sum(is.na(df[[col]]))
  return(missing_vals)
}
# Test
data <- data.frame(x = 1:5, y = c(1, NA, NA, 4, 4))
data

A2_q3(data,"y")

# Q4a

# Simulations
number_of_sims <- 100000

# Simulate X and Y
X_parta <- rnorm(number_of_sims,mean=0,sd=2)
Y_parta <- rexp(number_of_sims,rate=3)

# Number of times X > Y 
comparison <- mean(X_parta > Y_parta)

comparison

# Q4b

X_partb <- rnorm(number_of_sims,mean=2,sd=1)
Y_partb <- rexp(number_of_sims,rate=2)

# Calculate min(X, Y) for each simulation
min_XY <- pmin(X_partb, Y_partb)

# Calculate the average
avg_min_XY <- mean(min_XY)

avg_min_XY

# Q5
S <- 0

# Iteration of numbers through 1 to 100
for (i in 1:100){
  # Add or subtract based on whether i is odd or even
  if (i %% 2 == 0){
    S <- S - i^2
  } else {
    S <- S + i^2
  }
}
print(S)

# Q6

data(mtcars)

# finding the subset of mtcars such that the cars have exactly 4 forward gears
subset_data <- filter(mtcars, gear == 4)
print(subset_data)

# Q7a
# Using filter to find the subset of mtcars such that the cars have 6 cylinders or 4 forward gears.
subset_data_7a <- nrow(filter(mtcars,cyl == 6 | gear == 4)) 
subset_data_7a

# Q7b
#Using filter to find the subset of mtcars such that the cars have 6 cylinders and 4 forward gears.
subset_data_7b <- nrow(filter(mtcars,cyl == 6, gear == 4))
subset_data_7b

# Q8
# Sorting cars based on weight
sorted_cars <- arrange(mtcars,mpg)
print("Heaviest car is: ")
print(sorted_cars[nrow(sorted_cars), ])

print("Lightest car is: ")
print(sorted_cars[1, ])

# Q9
# Flights departed in Jan or Nov such that the flights were not delayed on arrival by more than 10 minutes
data(flights)
flights_filtered <- filter(flights, month == 1 | month == 11, arr_delay <= 10)
flights_filtered

# Q10

# Filter flights departing in May
flights_in_may <- filter(flights,month == 5)

# Count flights departing in May
may_flights_count <- nrow(flights_in_may)

may_flights_count

# Q11a

# Store mean
mean_values <- numeric(ncol(mtcars))

# Iterate through the column
for (i in 1:ncol(mtcars)){
  mean_values[i] <- mean(mtcars[[i]])
}

mean_values

# Q11b
values <- numeric(ncol(iris))

# Iterate through the column in iris
for (i in 1:ncol(iris)){
  values[i] <- length(unique(iris[[i]]))
}

values

# Q12a

# Compute the mean of every column in mtcars data 
mean_value_12a <- apply(mtcars,2,mean)

mean_value_12a

# Q12b
# Unique values of a vector
unique_vals <- function(x){
  length(unique(x))
}

# Unique function for each column of iris
vals_unique_count <- apply(iris,2,unique_vals)
vals_unique_count

# Q13
set.seed(1)
x <- runif(100)
output <- cumsum(x)
output

# Q14

preallocation <- function(){
  output <- rep(0,10000)
  for (i in 1:10000){
    output[i] <- 1
  }
}

no_preallocation <- function(){
  output <- rep()
  for (i in 1:10000){
    output <- c(output,1)
  }
}

preallocation_time <- system.time(preallocation())

no_preallocation_time <- system.time(no_preallocation())

print("Time for preallocated output: ")
print(preallocation_time)

print("Time for non-preallocated output: ")
print(no_preallocation_time)

# We can see that preallocating output takes much lesser time from this experiment since resizing of the output would not be required

# Q15
sim_flip <- function(){
  result <- ""
  while (result != "HHH"){
    coin_flips <- sample(c("H","T"),3,replace = TRUE)
    result <- paste(coin_flips,collapse="")
    print(result)
  }
}
sim_flip()

# Q16

# Initialize
i <- 1

# While loop
while(i <= 10){
  print(i)
  i <- i+1
}

# Q17
getwd()

setwd("C:/Users/jayan/Downloads")
my_data <- read.csv("C:/Users/jayan/Downloads/assignment2_df.csv")

missing_values <- numeric(ncol(my_data))
column_names = colnames(my_data)
# Iterate through each column
for (col_index in 1:length(column_names)){
  col_sing_name <- column_names[col_index]
  missing_value_count <- 0
  
  for (row_index in 1:nrow(my_data)) {
    if(is.na(my_data[row_index,col_index])){
      missing_value_count <- missing_value_count + 1
    }
  }
  missing_values[col_index] <- missing_value_count
}
print(missing_values)

# Q18
# Find number of 5's in each row
fives_row <- rowSums(my_data == 5,na.rm = TRUE)
print(fives_row)

# Q19
new_colnames <- vector("character",length = ncol(my_data))

for (i in 1:length(new_colnames)) {
  new_colnames[i] <- paste("Q",i,sep="")
}
colnames(my_data) <- new_colnames
new_colnames

# Q20
my_data <- my_data[, order(colnames(my_data))]
my_data

