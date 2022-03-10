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
ind_func <- function(y,c){
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
softmax <- function(df, init = as.vector(rep(1, ncol(df)-1)), batch_num = NULL, exact = NULL){
  # initialize a data frame to store betas after every iteration
  # if (!is.null(exact)){
  #   # Add error column
  #   m <- matrix(NA, ncol = ncol(df), nrow = 1)
  # }
  # else{
  #   m <- matrix(NA, ncol = ncol(df)-1, nrow = 1)
  # }
  # 
  # temp_df <- data.frame(m, check.names = FALSE)
  # 
  # names <- c()
  # # make a vector of names for columns
  # for (i in 2:ncol(df)-2) {
  #   names <- append(names, paste0("Betas.", i))
  # }
  # if (!is.null(exact)){
  #   ## Add error column
  #   names <- append(names, "error")
  # }
  # # assign the names vector to the column names of temp data frame
  # # print(c(ncol(temp_df), length(names)))
  # colnames(temp_df) <- names
  # 
  # if (!is.null(exact)){
  #   ## Add error
  #   error = norm_L2(betas_prev - exact)
  #   temp_df <- na.omit(rbind(temp_df, c(betas_prev, error)))
  # }
  # else{
  #   temp_df[1,] <- betas_prev
  # }
  # Check for subsampling
  if (batch_num.isnull){
    iterations = nrow(df)
  }
  else{
    iterations = batch_num
  }
  
  betas = init
  
  batch <- df[sample(nrow(df), size=batch_num, replace=FALSE),]  # take a random subsample from the data
  X <- batch[,1:(ncol(df)-1)] # matrix of data values, ommitting targets
  Y <- batch[,ncol(df)] # vector of target values
  
  B_full = rep(0, ncol(df)-1)
  
  for(i in 1:batch_num){
    N = batch_num
    Dh = thetas_prev  #init
    sum = sum(exp(X[i], betas))
    p.k = prob_softmax(X[i], beta, sum)
    delta.i = ind_func(Y[i], c)
    s.i = delta.i - p.k
    derivative.1 = s.i %x% X[i] # This will be a K x N matrix, %x% := tensor product
    phi = p.k - p.k^2
    derivative.2 = phi %x% (X[i] %*% X[i])
    gradient = 1/N*sum(tensor_product[1:nrow(tensor_product)])
    
    B_full = B_full + inv(derivative.2) %*% derivative.1
  }
}

# Testing
p <- 5
n <- 1000
x <- matrix(rnorm(n * p), n, p)
x=cbind(1,x)
betas <- runif(p+1, -2, 2)
hc <- function(x) 1 /(1 + exp(-x)) # inverse canonical link
p.true <- hc(x %*% betas)
y <- floor(runif(n, min=0, max=7)) # produce n number of uniformly distributed numbers between 0 and 6 (given floor)
df <- cbind(x,y)
init=betas+rnorm(p+1,0,1) #add randomness

# print(init)
library(pracma)
# print(tail(softmax(df,init)))
#exact values
print(df)
print(betas)