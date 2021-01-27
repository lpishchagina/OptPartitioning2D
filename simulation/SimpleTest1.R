#Simple test :

devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)
set.seed (21)
n <- 10
chp <- c(3, 5, 8, 10)
mu1 <- c(0, 1, 0, 1)
mu2 <- c(1, 2, 3, 4)
sigma <- 1
penalty <- 2 * sigma * log(10)

data <- GenData2D(n, chp, mu1, mu2, sigma, sigma)
data
#           [,1]      [,2]      [,3]     [,4]     [,5]      [,6]      [,7]      [,8]     [,9]    [,10]
#[1,]  0.7930132 0.5222513 1.7462222 -0.2713361 3.197390 0.4331308 -1.570200 -0.9349057 1.063493 0.9976067
#[2,] -1.2767812 1.7574122 0.4515944  2.1725495 2.562853 4.5118180  3.659025  4.1220281 3.215359 3.5743077
PlotOptPart2D(data, OptPart$changepoints,OptPart$means1, OptPart$means2)

OptPart <- OptPart2D(data[1,], data[2,], penalty, type="null")
OptPart
#$changepoints
#[1]  1  4  5 10
#$means1
#[1]  0.793013171  0.665712461  3.197389533 -0.002174902
#$means2
#[1] -1.276781  1.460519  2.562853  3.816508
#$globalCost
#[1] 24.16975

OptPELT <- OptPart2D(data[1,], data[2,], penalty, type="pruning")
OptPELT
#$changepoints
#[1]  4  5 10
#$means1
#[1]  0.697537638  3.197389533 -0.002174902
#$means2
#[1] 0.7761937 2.5628531 3.8165075
#$globalCost
#[1] 25.19634

