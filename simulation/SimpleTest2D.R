#Verification of the OptPartitioning and PELT algorithms using the simplest examples

devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)

#Test1: Values of mean is constant over the interval (0,T1_n).
T1_n <- 500
T1_chp <- T1n
T1_mu1 <- 0
T1_mu2 <- 1
T1_sigma <- 1
T1_penalty <- 2 * T1_sigma * log(T1_n)

T1_data <- GenData2D(T1_chp, T1_mu1, T1_mu2, T1_sigma, T1_sigma)

T1_OptPart <- OptPart2D(T1_data[1,], T1_data[2,], T1_penalty, type="null")
PlotOptPart2D(T1_data, T1_OptPart$changements,T1_OptPart$means1, T1_OptPart$means2)

T1_PELT <- OptPart2D(T1_data[1,],T1_data[2,], T1_penalty, type="pruning")
PlotOptPart2D(T1_data, T1_PELT$changements,T1_PELT$means1, T1_PELT$means2)

#Test2: Data has 2 segments with values of means.
T2_n <- 500
T2_chp <- c(T2_n/2, T2_n)
T2_mu1 <- c(0, 1)
T2_mu2 <- c(1, 0)
T2_sigma <- 1
T2_penalty <- 2 * T2_sigma * log(T2_n)

T2_data<-GenData2D(T2_chp, T2_mu1,T2_mu2,T2_sigma,T2_sigma)

T2_OptPart<-OptPart2D(T2_data[1,], T2_data[2,],T2_penalty, type="null")
PlotOptPart2D(T2_data, T2_OptPart$changepoints,T2_OptPart$means1, T2_OptPart$means2)

T2_PELT <- OptPart2D(T2_data[1,], T2_data[2,], T2_penalty, type="pruning")
PlotOptPart2D(T2_data, T2_PELT$changepoints,T2_PELT$means1, T2_PELT$means2)

#Test3:The data has multiple segments.
T3_n <- 500
T3_chp <- seq(from=100, to=T3_n,by=100)
T3_mu1 <- c(1:5)
T3_mu2 <- c(0, 1, 0, 1, 0)
T3_sigma <- 1
T3_penalty <- 2 * T3_sigma * log(T3_n)

T3_data <- GenData2D(T3_chp, T3_mu1,T3_mu2,T3_sigma,T3_sigma)

T3_OptPart<-OptPart2D(T3_data[1,], T3_data[2,],T3_penalty, type="null")
PlotOptPart2D(T3_data, T3_OptPart$changepoints,T3_OptPart$means1, T3_OptPart$means2)

T3_PELT <- OptPart2D(T3_data[1,], T3_data[2,], T3_penalty, type="pruning")
PlotOptPart2D(T3_data, T3_PELT$changepoints,T3_PELT$means1, T3_PELT$means2)

#Test4: Data1 and data2 have different number of segments.
T4_n <- 500
T4_chp <- seq(from = 100, to = T4_n, by = 100)
T4_mu1 <- c(1:5)
T4_mu2 <- c(0, 0, 5, 1, 0)
T4_sigma <- 1
T4_penalty <- 2 * T4_sigma * log(T4_n)

T4_data <- GenData2D(T4_chp, T4_mu1,T4_mu2,T4_sigma,T4_sigma)

T4_OptPart<-OptPart2D(T4_data[1,], T4_data[2,],T4_penalty, type="null")
PlotOptPart2D(T4_data, T4_OptPart$changepoints,T4_OptPart$means1, T4_OptPart$means2)

T4_PELT <- OptPart2D(T4_data[1,], T4_data[2,], T4_penalty, type="pruning")
PlotOptPart2D(T4_data, T4_PELT$changepoints,T4_PELT$means1, T4_PELT$means2)

