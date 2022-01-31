# Testing

library(ggplot2)

#Initialize synthetic data
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
init=betas+rnorm(p+1,0,1)

# print(tail(trunc_stochastic_newton_algo(df,init, 1/4, 1/2)))
# print(tail(stochastic_newton_algo(df,init)))
output.df <- stochastic_newton_algo(df,init, exact = betas)
output.df1 <- trunc_stochastic_newton_algo(df,init, exact = betas, 1/4, 1/2) # What do these constants do?
# set nrow to align with newton algos that are limited by number of rows
output.df2 <- sgd2(df, init, num_iter = nrow(df), exact = betas)
## DELETE LAST LINE OF output.df SINCE ALL 0s (ISSUE)
output.df <- head(output.df, -1)
output.df1 <- head(output.df1, -1)
output.df2 <- head(output.df2, -1)

error <- output.df[, ncol(output.df)]

error1 <- output.df1[, ncol(output.df1)]

error2 <- output.df2[, ncol(output.df2)]

# Make the plots
n <- seq(1, n, by = 1)
p <- ggplot()+
  geom_line(aes(n, y = error), color = "forestgreen") + 
  geom_point(aes(n, y = error), color = "forestgreen", shape = 15, size = .25) +
  geom_line(aes(n, y = error1), color = "orange") + 
  geom_point(aes(n, y = error1), color = "orange", shape = 15, size = .25) + 
  geom_line(aes(n, y = error2), color = "salmon") + 
  geom_point(aes(n, y = error2), color = "salmon", shape = 15, size = .25) + 
  ggtitle("Error plot")
print(p)

p <- ggplot() + 
  geom_boxplot(aes(x = "SNA", y=error), outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  geom_boxplot(aes(x = "TSNA", y=error1), outlier.colour="red", outlier.shape=8,
               outlier.size=1) +
  geom_boxplot(aes(x = "SGD", y=error2), outlier.colour="red", outlier.shape=8,
             outlier.size=1)
print(p)

