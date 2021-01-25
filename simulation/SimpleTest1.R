#Simple test :

devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)
n <- 10
chp <- c(3, 5, 8, 10)
mu1 <- c(0, 1, 0, 1)
mu2 <- c(1, 2, 3, 4)
sigma <- 1
penalty <- 2 * sigma * log(10)

data <- GenData2D(n, chp, mu1, mu2, sigma, sigma)
data

PlotOptPart2D(data, chp,mu1, mu2)

OptPart <- OptPart2D(data[1,], data[2,], penalty, type="null")
OptPart
