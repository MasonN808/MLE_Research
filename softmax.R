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
softmax <- function(df, K, init = matrix(1, ncol(df)-1, nrow(df)-1), batch_num = nrow(df), eta = .01, exact = NULL, DEBUG = FALSE){
  B_full = init
  if (!is.null(exact)){
    # Add error column
    m <- matrix(NA, ncol = ncol(df), nrow = 1)
  }
  else{
    m <- matrix(NA, ncol = ncol(df)-1, nrow = 1)
  }

  temp_df <- data.frame(m, check.names = FALSE)

  names <- c()
  # make a vector of names for columns
  if (!is.null(exact)){
    # Add error column
    names <- append(names, "error")
  }
  
  # assign the names vector to the column names of temp data frame
  colnames(temp_df) <- names

  if (!is.null(exact)){
    # Add error
    error = 0
    for (i in 1: length(B_full[1,])) {
      error = error + norm_L2(B_full[,i] - exact[,i])
    }
    
    error = error/length(B_full[1,])
    # temp_df <- na.omit(rbind(temp_df, c(B_full, error)))
    temp_df[1,] <- error
  }
  else{
    temp_df[1,] <- B_full
  }
  
  X <- df[,1:ncol(df)-1]
  if(DEBUG){
    cat(paste("X: \n-------------------------- \n"))
    print(X)
    cat(paste("-------------------------- \n"))
  }
  
  Y <- df[,ncol(df)]
  if(DEBUG){
    cat(paste("Y: \n-------------------------- \n"))
    print(Y)
    cat(paste("-------------------------- \n"))
  }
  
  # B_full = matrix(0, ncol = K, nrow = d+1)
  # print((B_full))
  
  N = nrow(df)
  k = 1:(K-1)
  j = 1:N
  for(i in 1:N){ # Looping through each row in the df
    if(DEBUG){
      cat(paste("B_full: \n-------------------------- \n"))
      print(B_full)
      cat(paste("-------------------------- \n"))
    }

    # Reinitialize variables
    p.k.vec = c()
    s.i.vec = c()
    sum = 0
    sum = 1 + sum(exp(X[i,] %*% B_full[,k]))
    for (j in 1:(K-1)) { # Looping through each column in the Betas to populate s.i.vec and p.k.vec
      p.k = prob_softmax(X[i,], B_full[,j], sum) # K dimensional vector : betas[,j] d dimesional : X[i,] d dimensional
      p.k.vec <- c(p.k.vec, p.k)
      
      #This tells us if our predication using B_full is correct using the indicator function
      delta.i = ind_func(Y[i], j) # 0 or 1
      s.i = delta.i - p.k # K dimensional vector 
      s.i.vec <- c(s.i.vec, s.i)
    }
    
    if(DEBUG){
      cat(paste("p.k.vec: \n-------------------------- \n"))
      print(p.k.vec)
      cat(paste("-------------------------- \n"))
      
      cat(paste("s.i.vec: \n-------------------------- \n"))
      print(s.i.vec)
      cat(paste("-------------------------- \n"))
    }

    derivative.1 = (1/N)*(s.i.vec %x% (X[i,])) # This will be a K x d matrix, %x% := tensor product
    
    if(DEBUG){
      cat(paste("derivative.1: \n-------------------------- \n"))
      print(derivative.1)
      cat(paste("-------------------------- \n"))
    }
    
    # initiate phi matrix of just 0s
    phi = matrix(rep(0, (K-1)*(K-1)), nrow = K-1, byrow = TRUE)
    
    # Populate the phi matrix
    for(j in 1:(K-1)){
      for(k in 1:(K-1)){
        # print(p.k.vec[k]*p.k.vec[j])
        phi[k,j] = -p.k.vec[k]*p.k.vec[j] # replace the elements in row k, column j
        # cat(paste("phi: ", phi, "\n", "--------------------------", "\n"))
      }
    }
    
    tempP.k.vec <- p.k.vec
    
    diag(phi) <- tempP.k.vec - (tempP.k.vec*tempP.k.vec) # faster than just squaring using ^2
    if(DEBUG){
      cat(paste("PHI: \n-------------------------- \n"))
      print(phi)
      cat(paste("-------------------------- \n"))
    }
    
    derivative.2 = -(1/N)*(phi %x% (X[i,] %*% t(X[i,]))) # Kd x Kd matrix
    
    epsilon = 10^(-8)
    
    diag(derivative.2) = diag(derivative.2) + epsilon # Make it nonsingular
    
    if(DEBUG){
      print(size(derivative.1))
      print(size(inv(derivative.2)))
      print(size(B_full))

      cat(paste("Hessian \n -------------------------- \n"))
      print(derivative.2)
      cat(paste("-------------------------- \n"))
    }
    
    B_full = B_full - eta*matrix((inv(derivative.2)) %*% derivative.1, nrow=(ncol(df)-1), ncol = K-1) #put into matrix since it prints out as array
    # B_full = cbind(0, B_full)
    
    if (!is.null(exact)){
      ## Add error
      error = 0
      for (i in 1: length(B_full[1,])) {
        error = error + norm_L2(B_full[,i] - exact[,i])
      }
      error = error/length(B_full[1,])
      # temp_df <- na.omit(rbind(temp_df, c(B_full, error)))
      temp_df <- rbind(temp_df, error)
    }
    else{
      temp_df <- rbind(B_full)
    }

  }
  if(DEBUG){
    print((temp_df))
  }

  plot(1:length(temp_df$error), temp_df$error, col = "red")
  print(B_full)
  return(temp_df)
}

# Testing
K <- 2 # Number of classes
d <- 5 # Number of columns/features
n <- 5000 # Number of rows
x <- matrix(rnorm(n * (d-1)), n, d-1)
x <- cbind(1,x)

yBetas <- matrix(rep(1), ncol = K-1, nrow = d)


yTargets <- c()
for(i in 1:n){ # Looping through each row in the dataset
  k = 1:(K-1)
  j = 1:n
  
  sum = 0
  sum = 1 + sum(exp(x[i,] %*% yBetas[,k]))

  p.k.vec = c()
  for (j in 1:(K-1)) { # Looping through each column in the Betas
    p.k = prob_softmax(x[i,], yBetas[,j], sum) # K dimensional vector : betas[,j] d dimesional : X[i,] d dimensional
    p.k.vec <- c(p.k, p.k.vec)
  }
  p.k.vec <- c(1-sum(p.k.vec), p.k.vec)
  # print(sum(p.k.vec))
  yTargets <- c(yTargets, which(p.k.vec == max(p.k.vec))-1) # find the index with the max probability and minus 1 for indexing
}

df <- cbind(x,yTargets)

init <- matrix(rep(-1), ncol = K-1, nrow = d)

print(init)

library(pracma)

etas = c(.1, .05, .01, .001, .0001)
etas = rep(10^(-2), 5)
errors = c()
for(eta.i in etas){
  df.errors = softmax(df, K, init, eta = eta.i, exact = yBetas, DEBUG = FALSE)$Error # Get the error column
  error.i = df.errors[length(df.errors)]
  # print(error.i)
  errors = c(errors, error.i)
}
print(tail(softmax(df, K, init, eta = eta.i, exact = yBetas, DEBUG = FALSE)))
# print(errors)

#exact values
# print(yBetas)

