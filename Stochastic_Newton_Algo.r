stochastic_newton_algo <- function(df, iterations = 10){
  thetas_prev <- rep(1, ncol(df)) #initialize vector of thetas assigned 1 (This can 
                              #be changed upon initiation) with length of 
                              #number of columns from df
  S_n_inv_prev = diag(ncol(df)-1)
  for(i in 0:iterations){
    PHI <- df[i %% iterations, 0:(ncol(df)-1)] #take the ith row in df for every
                                              #instance in the sequence, if
                                              #i>iterations, use mod function
                                              #and go through df again until
                                              #termination
    exponent <- thetas_prev %*% PHI #calculate the exponent of the logistic function
    PI <- exp(exponent)/(1+exp(exponent)) #logistic function
    a_n <- PI*(1-PI)
    print(dim(PHI))
    print(dim(S_n_inv))
    S_n_inv <- S_n_inv_prev - a_n*solve(1+a_n*(t(PHI) %*% S_n_inv_prev %*% PHI))*S_n_inv_prev %*%
      PHI %*% t(PHI) %*% S_n_inv_prev
    thetas <- thetas_prev + S_n_inv %*% PHI*(df[i %% iterations, ncol(df)] - PI)
    thetas_prev <- thetas #unnecessary, but good for clarity
    print(thetas_prev)
  }

}

rm(list=ls())
library(optimx)
#set.seed(123)
p <- 5
n <- 10000
x <- matrix(rnorm(n * p), n, p)
x=cbind(1,x)
betas <- runif(p+1, -2, 2)
hc <- function(x) 1 /(1 + exp(-x)) # inverse canonical link
p.true <- hc(x %*% betas)
y <- rbinom(n, 1, p.true)
df <- cbind(x,y)
head(df)

stochastic_newton_algo(df, 10)

