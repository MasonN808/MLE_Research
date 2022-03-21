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
softmax <- function(df, K, init = matrix(1, ncol(df)-1, nrow(df)-1), batch_num = nrow(df), exact = NULL){
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
  # if (batch_num.isnull()){
  #   iterations = nrow(df)
  # }
  # else{
  #   iterations = batch_num
  # }
  print(init)
  betas = init
  
  batch <- df[sample(nrow(df), size=batch_num, replace=FALSE),]  # take a random subsample from the data
  X <- batch[,1:(ncol(df)-1)] # matrix of data values, ommitting targets
  Y <- batch[,ncol(df)] # vector of target values
  
  B_full = rep(0, ncol(df)-1)
  
  for(i in 1:batch_num){
    N = batch_num
    k = 1:K
    j = 1:N

    cat(paste("x[i,]: ", x[i,], "\n"))
    # print(betas)
    # print(betas[k])
    sum = sum(exp(X[i,] %*% betas[,k]))

    for (j in 1:K) {
      p.k = prob_softmax(X[i,], betas[,j], sum) # d dimensional vector
      cat(paste("p.k: ", p.k, "\n"))
      
      delta.i = ind_func(Y[i], j)
      cat(paste("delta.i: ", delta.i, "\n"))
      
      s.i = delta.i - p.k # K dimensional vector //make this its own method 
      cat(paste("s.i: ", s.i, "\n", "--------------------------", "\n"))
      
      #CONTINUE HERE 3/21
      
      derivative.1 = 1/N*(s.i %x% X[i]) # This will be a K x N matrix, %x% := tensor product
      cat(paste("derivative.1: ", derivative.1, "\n", "--------------------------", "\n"))
      
      phi = p.k - p.k^2 # K by K dimensional matrix
      cat(paste("phi: ", phi, "\n", "--------------------------", "\n"))
      
      derivative.2 = -1/N*(phi %x% (X[i] %*% X[i]))
      cat(paste("derivative.2: ", derivative.2, "\n", "--------------------------", "\n"))
      
      B_full = B_full + inv(derivative.2) %*% derivative.1
    }
  }
  return(B_full)
}

# Testing
K <- 7 # Start at K=2 first 3/21
d <- 5
n <- 100
x <- matrix(rnorm(n * d), n, d)
x=cbind(1,x)
targets <- runif(d+1, -2, 2)
hc <- function(x) 1 /(1 + exp(-x)) # inverse canonical link
d.true <- hc(x %*% targets)
y <- floor(runif(n, min = 0, max = K)) # produce n number of uniformly distributed numbers between 0 and 6 (given floor)
df <- cbind(x,y)
# init <- cbind(rep(0, nrow(df)-1), matrix(targets+rnorm(d+1,0,1), ncol = K, nrow = d+1))  #K by d dimensional matrix //TODO: make it MORE random (3/20)
init <- cbind(rep(0, nrow(df)-1), matrix(1, ncol = K, nrow = d+1))  #K by d dimensional matrix //TODO: make it MORE random (3/20)

# print(init)
library(pracma)
# print(df)
print(cat("targets: ", targets))
print(cat("test: ", sum(targets+rnorm(p+1,0,1))))
print(cat("OUT: ", tail(softmax(df, K, init))))
#exact values


