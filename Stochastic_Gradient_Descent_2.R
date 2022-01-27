#' Implementation of the stochastic gradient descent
#' 
#' @param df A data frame.
#' @param eta Learning rate
#' @param num_iter number of iterations
#' @param batch_num number of data rows in the batch (specifically for SGD)
#' @return theta
sgd <- function(df, eta = .005, num_iter = 50, batch_num = 10){
  # TODO: check if batch_num is less than number of rows in df
  # TODO: make error vector
  thetas_prev = as.vector(rep(1, ncol(df)-1))  # initializing weights
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
    # NOTE: batch works
    batch <- df[sample(nrow(df), size=batch_num,replace=FALSE),]  # take a random batch from the data
    # NOTE: values works
    values <- batch[ , 1:(ncol(df)-1)] # matrix of data values, ommitting targets
    # NOTE: targets works
    targets <- batch[ , ncol(df)] # vector of target values
    
    Dw = w  #initialize gradient of Loss w.r.t. weights
    Db = b  #initialize gradient of Loss w.r.t. bias
    
    loss <- 0
    y_preds <- c()   #initialize empty vector of predicted targets
    
    for (i in 1:batch_num){   #iterate through each row of data in the batch
      
      PHI <- values[i,] #take the ith row in df for every instance in the sequence
      # print(PHI)
      exponent <- thetas_prev %*% PHI #calculate the exponent of the logistic function
      PI <- exp(exponent)/(1+exp(exponent)) #logistic function
      
      Dh <- ((exponent*PHI)/(1+exponent)) - targets  #recalculate gradients using data, use t() for transpose
      # print(size(values[i,]))
      # print(size(t(w)))
      # print(size(targets[i]))
      # print(is.matrix(values[i,]))
      # print(is.matrix(t(w)))
      # print(is.matrix(targets[i]))
      Db <- (-2/batch_num ) * (targets[i] - drop(values[i,] %*% drop(w)) - b)  #recalculate gradients using data, use t() for transpose
      
      w <- w - eta * Dw   #recalculate weights using updated gradient
      b <- b - eta * Db   #recalculate intercept using updated gradient
      
      y_pred <- values[i,] %*% w   #calculate predicted target value
      # TODO: y_preds is too long <- caused by w
      y_preds <- c(y_preds, y_pred)  #append predicted value to predicted values vector
    }
    # print(y_preds)
    loss <- mean_squared_error(y_preds, targets)  #calculate MSE as loss
    print(loss)
    
    # print(cat(epoch, loss))
    epoch <- epoch + 1  # Go to next epoch
  }
  return(c(w,b))
}
