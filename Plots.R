library(latex2exp)
x <- seq(-10,10,.1)
y <- x^2

#plot the first data series using plot()
plot(x, y, type="o", col="black", pch=".", ylab=TeX(r'($x^2$)'), lty=1)
points(0, 0, col="forest green",pch=19, lwd=5)
points(c(-6.5,-6.5), c(0,100), col="red",pch=".")
lines(c(-6.5,-6.5), c(0,100), col="red", lty=2)
points(c(6.5,6.5), c(0,100), col="blue",pch=".")
lines(c(6.5,6.5), c(0,100), col="blue", lty=2)
# text(locator(), labels = c("a", "b"))

x <- seq(-5, 5, 0.01)
plot(x, exp(x)/(1+exp(x)), pch=".", lty=1, ylab = TeX(r'($e^x/{1+e^x})'))
