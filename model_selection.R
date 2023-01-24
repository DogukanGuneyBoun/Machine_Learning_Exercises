### A simple model selection exercise: Fitting a polynomial to the data ###

library(ggplot2)
library(splines)

# set a random seed to make the results reproducible

set.seed(123)
n_sample <- 100

# Train data 

x <- seq(from = 0, to = 12, length = n_sample)
fx <- sin(x) + exp((x-4)/5)

epsilon <- rnorm(n_sample, 0, sd = 2)
y <- fx + epsilon

# Test data 

ggplot(data = NULL, aes(x = x)) + geom_line(aes(y = fx), linewidth = 1) + geom_point(aes(y = y), color = "blue")


# Consider fitting a cubic spline with different number of knots. The true function is not a cubic spline, so we cannot expect to have an
# unbiased estimator, however a spline with enough d.o.f. should approximate well our function, and then have low bias. 

# To approximate bias and variance, we simulate many data sets and apply the same estimators to each data set, and we average. We then 
# obtain a graph of the results that tells us how squared bias and variance behave as a function of d.o.f. 

# set a random seed to make the results reproducible 
  
set.seed(123) 

n_sim <- 500
n_df <- 8 

# setup containers to store the results 

prediction_matrix <- matrix(NA, nrow = n_sim, ncol = n_sample)
mse_temp <- matrix(NA, nrow = n_sim, ncol = n_df)
msep_temp <- matrix(NA, nrow = n_sim, ncol = n_df)
naive_temp <- matrix(NA, nrow = n_sim, ncol = n_df)  # part 2.5 in the pdf

results <- matrix(NA, nrow = 5, ncol = n_df)

# outer for-loop
for (df_iter in 1:n_df) {
  # inner for-loop 
  for (mc_iter in seq(n_sim)) {
    epsilon_train <- rnorm(n_sample,0, sd = 2)
    y_train <- fx + epsilon_train
    
    cspline <- lm(y_train ~ splines::bs(x, df = df_iter + 2))
    cspline_predict <- predict(cspline, data.frame(x))
    prediction_matrix[mc_iter, 1:n_sample] <- cspline_predict
    mse_temp[mc_iter, df_iter] <- mean((cspline_predict - fx)^2)
    naive_temp[mc_iter, df_iter] <- mean((cspline_predict - y_train)^2)
  }
  var_matrix <- apply(prediction_matrix, 2, FUN = var) # pdf notes page 8 MSE_n formula first term
  bias_matrix <- apply(prediction_matrix, 2, FUN = mean) # pdf notes page 8 MSE_n formula second term
  squared_bias <- (bias_matrix - fx)^2
  results[1, df_iter] <- mean(var_matrix)  
  results[2, df_iter] <- mean(squared_bias)
}
results[3,1:n_df] <- apply(mse_temp, 2, FUN = mean)
results[4,1:n_df] <- apply(naive_temp, 2, FUN = mean)
results[5,1:n_df] <- (1:n_df) + 3
results <- t(results)
colnames(results) <- c("Var", "SquaredBias", "MSE", "Naive",
                       "Degrees_of_Freedom")
results <- data.frame(results)

library(tidyverse)
plabs <- c("Var", "SquaredBias", "MSE", "Degrees_of_Freedom")
presults <- results[plabs]
presults %>%
  pivot_longer(cols = - Degrees_of_Freedom) %>%
  ggplot(aes(Degrees_of_Freedom, value, color = name)) +
  geom_line()
