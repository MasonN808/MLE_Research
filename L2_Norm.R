#' Implementation of L2 norm
#' 
#' @param x A vector
#' @return a real number
norm_L2 <- function(x){
  return(sqrt(sum(x^2)))
}