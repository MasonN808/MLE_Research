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
head(df)
init=betas+rnorm(p+1,0,1)

# print(tail(trunc_stochastic_newton_algo(df,init, 1/4, 1/2)))
# print(tail(stochastic_newton_algo(df,init)))
output.df <- stochastic_newton_algo(df,init, exact = betas)

## DELETE LAST LINE OF output.df SINCE ALL 0s (ISSUE)
output.df <- head(output.df, -1)


n <- seq(1, n, by = 1)
p <- ggplot(output.df)+
  geom_line(aes(n, y = error), color = "forestgreen") + 
  geom_point(aes(n, y = error), color = "forestgreen", shape = 15, size = .25) + 
  ggtitle("Error plot")
print(p)

p <- ggplot(output.df, aes(x = "dfd", y=error)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)
print(p)