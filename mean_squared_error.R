#' Implementation of loss function: MSE (Mean Squared Error)
#' 
#' @param prediction A vector of predictions.
#' @param actual A vector of actual values
#' @return Real number
mean_squared_error <- function(prediction, actual){
  if (length(prediction) != length(actual)){
    print(cat("length of predictionVector: ",length(prediction)))
    print(cat("length of actualVector: ",length(actual)))
    return("Error: Length of prediction vector and actual vector are not of same size")
  }
  n = length(prediction)
  prev = 0  #Initialize loss
  for (i in 1: n){
    prev <- prev + (prediction[i] - actual[i])^2
  }
  return(prev/n)
}