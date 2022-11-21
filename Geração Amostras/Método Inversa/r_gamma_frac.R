

f <- function(x) pgamma(x, shape = 0.2, rate = 1/3)


library(GoFKernel)
g.inv <- inverse(f, lower = 0, upper = 100)


x <- replicate(1000, g.inv(runif(1)))


fitdistrplus::fitdist(x, 'gamma', 'mge')
