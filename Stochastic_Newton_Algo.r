stochastic_newton_algo <- function(df, iterations = 10){
  thetas <- rep(1, ncols(df)) #initialize vector of thetas assigned 1
  exponent <- thetas %*%..................
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
s
stochastic_newton_algo(df, 10)

