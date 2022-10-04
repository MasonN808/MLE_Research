# Testing

library(ggplot2)
source("Batched_Stochastic_Gradient_Descent.R")
source("L2_Norm.R")
source("mean_squared_error.R")
source("min_max_normalization.R")
source("softmax_with_stochastic_newton.R")
source("softmax.R")
source("Stochastic_Gradient_Descent_2.R")
source("Stochastic_Newton_Algo.R")
source("trunc_stochastic_newton_algo.R")

#Initialize synthetic data
p <- 5
n <- 5000
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
output.df1 <- trunc_stochastic_newton_algo(df,init, exact = betas, CONSTANT = 10^(-10), BETA = .49)
# set nrow to align with newton algos that are limited by number of rows
output.df2 <- sgd2(df, init, eta = .01, num_iter = n, exact = betas)
output.df3 <- sgd_batched(df, init, eta = .01, num_iter = n, batch_num = 10, exact = betas)
## DELETE LAST LINE OF output.df SINCE ALL 0s (ISSUE)
output.df <- head(output.df, -1)
output.df1 <- head(output.df1, -1)
output.df2 <- head(output.df2, -1)
output.df3 <- head(output.df3, -1)

error <- output.df[, ncol(output.df)]

error1 <- output.df1[, ncol(output.df1)]

error2 <- output.df2[, ncol(output.df2)]

error3 <- output.df3[, ncol(output.df3)]

# Make the plots
n <- seq(1, n, by = 1)
# p <- ggplot()+
#   geom_line(aes(n, y = error), color = "forestgreen") +
#   geom_point(aes(n, y = error), color = "forestgreen", shape = 15, size = .25) +
#   geom_line(aes(n, y = error1), color = "orange") +
#   geom_point(aes(n, y = error1), color = "orange", shape = 15, size = .25) +
#   geom_line(aes(n, y = error2), color = "salmon") +
#   geom_point(aes(n, y = error2), color = "salmon", shape = 15, size = .25) +
#   geom_line(aes(n, y = error3), color = "blue") +
#   geom_point(aes(n, y = error3), color = "blue", shape = 15, size = .25) +
#   scale_colour_brewer(palette = "Set1", limits = c("1", "2", "3", "4"), guide = "none") +
#   ggtitle("Error plot")

#plot the first data series using plot()
plot(n, error, type="o", col="forestgreen", pch=".", ylab="Error", lty=1)

#add second data series to the same chart using points() and lines()
points(n, error1, col="orange", pch=".")
lines(n, error1, col="orange",lty=1)

#add third data series to the same chart using points() and lines()
points(n, error2, col="dark red",pch=".")
lines(n, error2, col="dark red", lty=1)

#add fourth data series to the same chart using points() and lines()
points(n, error3, col="blue",pch=".")
lines(n, error3, col="blue", lty=1)

legend(x="right",legend=c("SNA","TSNA","SGD", "BSGD"), col=c("forestgreen","orange","dark red", "blue"),
       pch=c(".",".",".","."),lty=c(1,1,1,1), ncol=1)

# now extract the legend
#legend <- get_legend(p)

# and replot suppressing the legend
# p <- p + theme(legend.position='none')
print(p)

p <- ggplot() + 
  geom_boxplot(aes(x = "SNA", y=error), outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  geom_boxplot(aes(x = "TSNA", y=error1), outlier.colour="red", outlier.shape=8,
               outlier.size=1) +
  geom_boxplot(aes(x = "SGD", y=error2), outlier.colour="red", outlier.shape=8,
             outlier.size=1) +
  geom_boxplot(aes(x = "BSGD", y=error3), outlier.colour="red", outlier.shape=8,
               outlier.size=1)
print(p)

