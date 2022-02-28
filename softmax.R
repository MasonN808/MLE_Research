#' Probability function for y=c_k given data vector
#' 
#' @param x a d dimensional covariate vector.
#' @param sum sum of e^(x %*% Betas) Betas: d dimensional regression coefficients belonging to a compact subset of R^d (Kxd dimensional matrix)
#' @param Beta d dimensional regression coefficient
#' @return Betas
prob_softmax <- function(x, Beta, sum){
  out <- exp(x %*% Beta)/sum
  return(out)
}

#' Indicator Function
#' 
#' @param y a number
#' @param c a number from possible target variables
#' @return 1 or 0
ind_func <- function(x, Beta, sum){
  if (y == c){
    out = 1
  }
  else{
    out = 0
  }
  return(out)
}

#' Implementation of softmax regression
#' 
#' @param df A data frame.
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @param batch_num number of data rows in the subsample
#' @return theta
softmax <- function(df, init = as.vector(rep(1, ncol(df)-1)), eta = .001, num_iter = 1000, batch_num = 30, exact = NULL){
  betas = init  # initializing weights
  while(i < num_iter){
    sum = sum(exp(x, betas))
    p.k = prob_softmax(x, beta, sum)
    delta.i = ind_func(y, c)
    s.i = delta.i - p.k
    tensor_product = s.i %x% p.k #This will be a K x N matrix
    gradient = 1/N*sum(tensor_product[1:nrow(tensor_product)])
    i <- i + 1
  }
}