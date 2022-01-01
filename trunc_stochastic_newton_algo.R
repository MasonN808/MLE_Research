#' Implementation of the truncated stochastic Newton algorithm
#' 
#' @param df A data frame.
#' @param thetas_prev A vector of initial theta values in the Reals.
#' @param exact A vector of exact values. If NULL, error column isn't outputted
#' @param CONSTANT A positive real number.
#' @param BETA A number in [0, 1/2].
#' @return A data frame of thetas after nrow(df) iterations.
trunc_stochastic_newton_algo <- function(df , CONSTANT, BETA, thetas_prev = rep(1, ncol(df)-1), exact = NULL){
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
    ## Add error column
    names <- append(names, "error")
  }
  # assign the names vector to the column names of temp data frame
  colnames(temp_df) <- names
  # insert first initial thetas from input and remove NAs when initiated temp_df(could be done differently to name columns?)
  # ERROR: row 1 is "2" and row 2 is "21"; likely due to this line below
  if (!is.null(exact)){
    ## Add error
    error = Norm(thetas_prev - exact, p = 2)
    temp_df <- na.omit(rbind(temp_df, c(thetas_prev, error)))
  }
  else{
    temp_df <- na.omit(rbind(temp_df, thetas_prev))
  }
  iterations = nrow(df)
  S_n_inv_prev = diag(ncol(df)-1)
  for(i in 1:iterations){
    PHI <- df[i, 1:(ncol(df)-1)] #take the ith row in df for every instance in the sequence
    exponent <- thetas_prev %*% PHI #calculate the exponent of the logistic function
    PI <- exp(exponent)/(1+exp(exponent)) #logistic function
    a_n <- drop(PI*(1-PI)) #Use drop to convert from 1x1 matrix to scalar
    
    # alph_n = max(a_n, constant/n^B) = max(1/4(cosh(PHI*thetas_prev/2))^2, constant/n^B)
    alpha_n <- max(c(a_n, CONSTANT/(i)**BETA))
    
    S_n_inv <- S_n_inv_prev - alpha_n * 1/(1 + alpha_n*drop((t(PHI) %*% S_n_inv_prev %*% matrix(PHI)))) * 
      S_n_inv_prev %*% matrix(PHI) %*% t(PHI) %*% S_n_inv_prev
    
    thetas <- thetas_prev + (S_n_inv %*% PHI) * drop((df[i %% nrow(df), ncol(df)] - PI))
    
    thetas_prev <- drop(thetas) #Use drop to drop extra dimension from thetas
    S_n_inv_prev <- S_n_inv
    
    if (!is.null(exact)){
      ## Add error
      error = Norm(thetas_prev - exact, p = 2)
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
#print(head(df))
init=betas+rnorm(p+1,0,1)

print(head(df))

print(tail(trunc_stochastic_newton_algo(df, 10^(-10), .49, init)))
print(betas)