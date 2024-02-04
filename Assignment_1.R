# Q1a

my_LS <- function(X,Y){
  # find the transpose of X according to the formula
  XT <- t(X)
  
  # Compute (X^T X)^-1
  XTX_inverse <- solve(XT %*% X)
  
  # Compute X^T Y
  XTY <- XT %*% Y
  
  # Find the least squares estimate
  estimate <- XTX_inverse %*% XTY 
  
  # Convert the result into a vector
  estimate_vector <- as.vector(estimate)
  
  return(estimate_vector)
}

# Testing 1a
X <- cbind(rep(1, 10), 1:10)
Y <- seq(1, 30, length = 10)
my_LS(X, Y)

# Q1b

my_ridge <- function(X,Y,lambda){
  # Compute the transpose of X
  XT <- t(X)
  
  # Compute lambda times I (identity matrix)
  matrix <- lambda * diag(ncol(X))
  
  # Finding the inverse of the product of XT and the matrix
  inverse_product = solve(XT %*% X + matrix)
  
  # Find X^T(Y)
  XTY <- XT %*% Y
  
  # Find the ridge estimate by combining everything above together
  ridge_estimate = inverse_product %*% XTY
  
  # Converting the result into a vector 
  ridge_estimate_vector <- as.vector(ridge_estimate)
  
  return(ridge_estimate_vector)
}

# Testing 1b
X <- cbind(rep(1, 10), 1:10)
Y <- seq(1, 30, length = 10)
lambda <- 1
result <- my_ridge(X, Y, lambda)
print(result)

# Q2a

my_sum <- function(n,m){
  result <- 0
  # Nested loops for the summation
  for (x in 1:n){
    for (y in 1:m){
      # Dividing the fraction
      numerator <- x^2 * sqrt(y)
      denominator <- abs(x-2) + y
      result <- result + numerator/denominator
    }
  }
  return(result)
}

# Testing 2a
n_value <- 5
m_value <- 6
result <- my_sum(n_value, m_value)
print(result)

# Q2b


my_sum2 <- function(n, m){
  
  x <- matrix(1:n, nrow = m, ncol = n, byrow = TRUE)
  y <- matrix(1:m, nrow = m, ncol = n, byrow = FALSE)
  
  numerator <- x^2 * sqrt(y)
  denominator <- abs(x-2) + y
  sum(numerator/denominator) 
  
}

# Testing 2b
answer <- my_sum2(n = 5,m = 6)
print (answer)

# Q3

sampled_integer <- floor(runif(1, 1, 5))
print(sampled_integer)

# Q4

# Set the probability of getting a Head
p_head <- 0.6

# Simulate 10 independent coin flips
result <- rbinom(10, 1, p_head)

# Print the results (1 for Head, 0 for Tail)
print(result)


# Q5

l_p <- function(x, p) {
  norm <- sum(abs(x)^p)^(1/p)
  return(norm)
}

# Testing Q5
x <- c(1, 2, 3, 4)
p <- 2
result <- l_p(x, p)
print(result)

# Q6

sum_odd <- function(v) {
  
  sum <- 0
  for (i in v){
    if(i %% 2 == 1){
      sum <- sum + i
    }
  }
  return (sum)
}

# Testing Q6
v <- c(1, 5, 9, 4)
output <- sum_odd(v)
print(output)

# Q7a

# set parameter
p <- 0.3

# generating x-values
x_val <- 1:10

# Find the pmf using dgeom function
pmf <- dgeom(x_val,p)

# Create a plot of the pmf
barplot(pmf)

# Q7b

# Rate parameter
rate_param <- 2

# Generate the x values from 0 to 3
x_vals <- seq(0,3,length=100)

# Find the pdf for each x value using dexp
pdf <- dexp(x_vals,rate_param)

# Plot the pdf 
plot(pdf,type="l")

# Q8

# Set parameters
simulations = 10000
below <- rep(0,simulations)
above <- rep(0,simulations)
mean = 0.0002
sd = 0.015

# Simulation for A
for (i in 1:simulations){
  price_A <- 100*exp(cumsum(rnorm(20,mean,sd)))
  below[i] <- min(price_A) < 95
}
A <- below
print(mean(A))

# Simulation for B
for (i in 1:simulations){
  price_B <- 100*exp(cumsum(rnorm(40,mean,sd)))
  above[i] <- max(price_B) > 101
}
B <- above
print(mean(B))

# intersection
c <- 1
for (i in 1:simulations){
  price_A <- 100*exp(cumsum(rnorm(20,mean,sd)))
  price_B <- 100*exp(cumsum(rnorm(20,mean,sd)))
  if (((below[i] <- min(price_A) < 95) == 1) && ((above[i]<-max(price_B) > 101) == 1 )){
    c <- c+1
  }
}
print(c)
probability <- c/simulations
cat("Probability of A intersecting B is ", probability)


# Q9

p_heads <- 0.7
rounds <- 100
number_wins <- 1
number_loses <- -1.5

# Coin flip simulations
coin_flip_sim <- rbinom(rounds, 1, p_heads)

# Calculate profit 
profit <- ifelse(coin_flip_sim == 1, number_wins, number_loses )

# Calculate cumulative profit
profit_cumulative <- cumsum(profit)

# Plot the cumulative profit vs. number of games
plot(1:rounds, profit_cumulative, type = "l")

# Q10
# Part 1: Ctrl + L for both windows/linux and mac

# Part 2: Ctrl + 2 for both windows/linux and mac

# Part 3: Esc for both windows/linux and mac

# Part 4: Ctrl + 1 for both windows/linux and mac

# Part 5: Ctrl + S for windows/linux and Cmd + S for mac

# Part 6: Ctrl + Enter for windows/linx and Cmd + Return for mac

# Part 7: Ctrl + Z for windows/linux and Cmd + Z for mac

# Part 8: Ctrl + X for windows/linux and Cmd + X for mac

# Part 9:  Ctrl + C for windows/linux and Cmd + C for mac

# Part 10: Ctrl + V for windows/linux and Cmd + V for mac
