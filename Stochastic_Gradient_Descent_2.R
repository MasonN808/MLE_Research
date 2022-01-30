#' Implementation of the stochastic gradient descent with loss function presented in "An stochastic ..."
#' 
#' @param df A data frame.
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @param batch_num number of data rows in the batch (specifically for SGD)
#' @return theta
sgd2 <- function(df, eta = .001, num_iter = 1000, batch_num = 30, exact = NULL){
  # TODO: check if batch_num is less than number of rows in df
  # TODO: make error vector
  thetas_prev = as.vector(rep(1, ncol(df)-1))  # initializing weights
  epoch = 1
  
  # m <- matrix(NA, ncol = ncol(df), nrow = 1)
  # temp_df <- data.frame(m, check.names = FALSE)
  # names <- c()
  # # make a vector of names for columns
  # for (i in 1:ncol(df)-1) {
  #   names <- append(names, paste0("Theta.", i))
  # }
  # colnames(temp_df) <- names
  # temp_df <- na.omit(temp_df)
  
  # initialize a data frame to store thetas after every iteration
  if (!is.null(exact)){
    ## Add error column
    m <- matrix(NA, ncol = ncol(df), nrow = 1)
  }
  else{
    m <- matrix(NA, ncol = ncol(df)-1, nrow = 1)
  }
  
  temp_df <- data.frame(m, check.names = FALSE)
  
  names <- c()
  # make a vector of names for columns
  for (i in 2:ncol(df)-2) {
    names <- append(names, paste0("Theta.", i))
  }
  if (!is.null(exact)){
    # Add error column
    names <- append(names, "error")
  }
  # assign the names vector to the column names of temp data frame
  colnames(temp_df) <- names
  # insert first initial thetas from input and remove NAs when initiated temp_df(could be done differently to name columns?)
  if (!is.null(exact)){
    # Add error
    error = norm(thetas_prev - exact)
    temp_df <- na.omit(rbind(temp_df, c(thetas_prev, error)))
  }
  else{
    temp_df <- na.omit(rbind(temp_df, thetas_prev))
  }
  
  while (epoch <= num_iter){
    # NOTE: batch works
    batch <- df[sample(nrow(df), size=batch_num,replace=FALSE),]  # take a random batch from the data
    # NOTE: values works
    X <- batch[ , 1:(ncol(df)-1)] # matrix of data values, ommitting targets
    # NOTE: targets works
    Y <- batch[ , ncol(df)] # vector of target values
    
    Dh = thetas_prev  #initialize gradient of Loss w.r.t. thetas
    # print(thetas_prev)
    
    for (i in 1:batch_num){   #iterate through each row of data in the batch
    
      PHI <- X[i,] #take the ith row in df for every instance in the sequence
      # print(typeof(thetas_prev))
      # print(typeof(PHI))
      exponent <- thetas_prev %*% PHI #calculate the exponent of the logistic function
  
      PI <- exp(exponent)/(1+exp(exponent)) #logistic function
      # print(size(PI))
      # print(size(PHI))
  
      Dh <- Dh + (PI %*% PHI) - (Y[i] * PHI)  #recalculate gradients using data, use t() for transpose
      # NOTE:
      # Y[i] * PHI computes
      # PI %*% PHI does not compute ==> PI is the culprit (maybe not) ==> converges to 1
      # Deleted PHI and computes, but not sure if computes correctly
      
      # print(exponent)
      
      # print(size(Dh))
      # print(size(targets[i]))
      # print(is.matrix(values[i,]))
      # print(is.matrix(t(w)))
      # print(is.matrix(targets[i]))
      thetas_prev <- thetas_prev - eta * Dh   #recalculate weights using updated gradient
      # print(thetas_prev)
      # print(i)

    }
    epoch <- epoch + 1  # Go to next epoch
    eta = eta / 1.02
    
    if (!is.null(exact)){
      # Add error
      error = norm(thetas_prev - exact)
      temp_df <- rbind(temp_df, c(thetas_prev, error))
    }
    else{
      temp_df <- rbind(temp_df, thetas_prev)
    }
  } 
  return(temp_df)
}


# Testing
p <- 5
n <- 10000
x <- matrix(rnorm(n * p), n, p)
x=cbind(1,x)
betas <- runif(p+1, -2, 2)
hc <- function(x) 1 /(1 + exp(-x)) # inverse canonical link
p.true <- hc(x %*% betas)
y <- rbinom(n, 1, p.true)
df <- cbind(x,y)
print(df)
init=betas+rnorm(p+1,0,1)

# Normalize the data
# df <- min_max_norm(df)

# print(init)
library(pracma)
print(sgd2(df))
#exact values
print(betas)
