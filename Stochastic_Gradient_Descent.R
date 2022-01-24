#' Implementation of the stochastic gradient descent
#' 
#' @param df A data frame.
#' @param init init_theta
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @return theta
sgd <- function(df, eta = .01, num_iter = 100, batch_num = 60){
  # TODO: check if batch_num is less than number of rows in df
  w = sample(1:ncol(df),1)  # Randomly initializing weights
  b = 1   # intercept value set to 1
  epoch = 1
  
  
  m <- matrix(NA, ncol = ncol(df), nrow = 1)
  temp_df <- data.frame(m, check.names = FALSE)
  names <- c()
  # make a vector of names for columns
  for (i in 1:ncol(df)-1) {
    names <- append(names, paste0("Theta.", i))
  }
  colnames(temp_df) <- names
  temp_df <- na.omit(temp_df)
  
  while (epoch <= num_iter){
    matrixToVector <- c(t(df))   #convert matrix to vector (sorted by rows)
                                 #TODO: Could be optimized here
    for(i in 1: length(matrixToVector)){
      print(matrixToVector[i])
    }
    batch <- sample(matrixToVector, batch_num)  # take a random batch from the data
    vectorToMatrix <- as.matrix(batch)  #convert vector back to matrix
    print(batch)
    values <- batch[, 1:(ncol(df)-1)] # matrix of data values, ommitting targets
    targets <- batch[,ncol(df)] # vector of target values
    
    Dw = w  #initialize gradient of Loss w.r.t. weights
    Db = b  #initialize gradient of Loss w.r.t. intercept
    
    loss <- 0
    y_preds <- c()   #initialize empty vector of predicted targets
    # sq_loss <- c()  # initialize 
    
    for (i in 1:batch_num){   #iterate through each row of data in the batch
      Dw <- (-2/batch_num * values[i]) * (targets[i] - values[i] %*% targets - b)   #recalculate gradients using data
      
      Db <- (-2/batch_num ) * (targets[i] - values[i] %*% targets - b)  #recalculate gradients using data
      
      w <- w - eta * Dw   #recalculate weights using updated gradient
      b <- b - eta * Db   #recalculate intercept using updated gradient
      
      y_pred <- values[i] %*% w   #calculate predicted target value
    }
    
    loss <- mean_squared_error(y_preds, targets)  #calculate MSE as loss
    
    print(c(epoch, loss))
    epoch <- epoch + 1  # Go to next epoch
  }
  return(w,b)
}
