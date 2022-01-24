#' Implementation of the stochastic gradient descent
#' 
#' @param df A data frame.
#' @param init init_theta
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @return theta
stochastic_newton_algo <- function(df, init, eta = .01, num_iter = 100){
  
  w = sample(1:13,1)  # Randomly initializing weights
  b = sample(1:1, 1)   # Random intercept value
  epoch = 1
  
  
  m <- matrix(NA, ncol = ncol(df), nrow = 1)
  temp_df <- data.frame(m, check.names = FALSE)
  names <- c()
  # make a vector of names for columns
  for (i in 1:ncol(df)-1) {
    names <- append(names, paste0("Theta.", i))
  }
  colnames(temp_df) <- names
  temp_df <- na.omit(rbind(temp_df, thetas_prev))
  
  while (epoch <= num_iter){
    
  }
  
 
  # iterations = nrow(df)
  
  ran <- sample(1:10, 1)
  
  for (i in 1:num_iter){
    
    grad = 
    theta = theta - (eta*grad)
  }
}
