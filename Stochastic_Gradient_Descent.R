#' Implementation of the stochastic gradient descent
#' 
#' @param df A data frame.
#' @param init init_theta
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @return theta
stochastic_newton_algo <- function(df, init, eta = .01, num_iter = 100){
  theta = init
  for (i in 1:num_iter){
    
    grad = 
    theta = theta - (eta*grad)
  }
}
