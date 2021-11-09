stochastic_newton_algo <- function(df, iterations = 10){
  thetas_prev <- rep(1, ncol(df)-1) #initialize vector of thetas assigned 1 (This can 
                              #be changed upon initiation) with length of 
                              #number of columns from df
  S_n_inv_prev = diag(ncol(df)-1)
  for(i in 1:iterations){
    PHI <- df[i %% iterations, 0:(ncol(df)-1)] #take the ith row in df for every
                                              #instance in the sequence, if
                                              #i>iterations, use mod function
                                              #and go through df again until
                                              #termination
    
    exponent <- thetas_prev %*% PHI #calculate the exponent of the logistic function
    PI <- exp(exponent)/(1+exp(exponent)) #logistic function
    a_n <- drop(PI*(1-PI)) #Use drop to convert from 1x1 matrix to scalar


    
    S_n_inv <- S_n_inv_prev - a_n * 1/(1 + a_n*drop((t(PHI) %*% S_n_inv_prev %*% matrix(PHI)))) * 
      S_n_inv_prev %*% matrix(PHI) %*% t(PHI) %*% S_n_inv_prev
    
    print(thetas_prev)
    print(dim(S_n_inv %*% PHI))
    print(dim(drop((df[i %% iterations, ncol(df)] - PI))))
    print(dim(PI))
    print(dim(df[i %% iterations, ncol(df)]))
    
    thetas <- thetas_prev + (S_n_inv %*% PHI) * drop((df[i %% iterations, ncol(df)] - PI))
    
    thetas_prev <- thetas #unnecessary, but good for clarity
    S_n_inv_prev <- S_n_inv
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

