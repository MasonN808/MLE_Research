

#' Implementation of min-max normalization
#' 
#' @param x A vector
#' @return a vector
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
