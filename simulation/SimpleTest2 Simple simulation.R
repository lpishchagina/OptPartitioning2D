#Verification of the Optimal Partitioning and PELT algorithms. The simplest examples.

devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)

set.seed (21)

#Test1: Values of mean is constant over the interval (0,T1_n) and sigma = 0.

T1_n <- 10
T1_chp <- T1_n
T1_mu1 <- 0
T1_mu2 <- 1
T1_sigma <- 0
T1_penalty <- 2 * T1_sigma * log(T1_n)

T1_data <- GenData2D(T1_n,T1_chp, T1_mu1, T1_mu2, T1_sigma, T1_sigma)
T1_data
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#[1,]    0    0    0    0    0    0    0    0    0     0
#[2,]    1    1    1    1    1    1    1    1    1     1
T1_OptPart <- OptPart2D(T1_data[1,], T1_data[2,], T1_penalty, type="null")
T1_OptPart
#$changepoints
#[1] 10
#$means1
#[1] 0
#$means2
#[1] 1
#$globalCost
#[1] 0

T1_PELT <- OptPart2D(T1_data[1,],T1_data[2,], T1_penalty, type="pruning")
T1_PELT
#$changepoints
#[1] 10
#$means1
#[1] 0
#$means2
#[1] 1
#$globalCost
#[1] 0

#Test2: Data has 2 segments with values of different means(sigma = 0).
T2_n <- 10
T2_chp <- c(T2_n/2, T2_n)
T2_mu1 <- c(0, 1)
T2_mu2 <- c(1, 0)
T2_sigma <- 0
T2_penalty <- 2 * T2_sigma * log(T2_n)
T2_data<-GenData2D(T2_n, T2_chp, T2_mu1,T2_mu2,T2_sigma,T2_sigma)
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#[1,]    0    0    0    0    0    1    1    1    1     1
#[2,]    1    1    1    1    1    0    0    0    0     0
T2_OptPart<-OptPart2D(T2_data[1,], T2_data[2,],T2_penalty, type="null")
T2_OptPart
#$changepoints
#[1]  5 10
#$means1
#[1] 0 1
#$means2
#[1] 1 0
#$globalCost
#[1] 0
PlotOptPart2D(T2_data, T2_OptPart$changepoints,T2_OptPart$means1, T2_OptPart$means2)

T2_PELT <- OptPart2D(T2_data[1,], T2_data[2,], T2_penalty, type="pruning")
T2_PELT
#$changepoints
#[1]  5 10
#$means1
#[1] 0 1
#$means2
#[1] 1 0
#$globalCost
#[1] 0
PlotOptPart2D(T2_data, T2_PELT$changepoints,T2_PELT$means1, T2_PELT$means2)

T2b_mu1 <- c(0, 1)
T2b_mu2 <- c(0, 1)
T2_sigma <- 0
T2_penalty <- 2 * T2_sigma * log(T2_n)

T2b_data<-GenData2D(T2_n, T2_chp, T2b_mu1,T2b_mu2,T2_sigma,T2_sigma)
#    [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#[1,]    0    0    0    0    0    1    1    1    1     1
#[2,]    0    0    0    0    0    1    1    1    1     1
T2b_OptPart<-OptPart2D(T2b_data[1,], T2b_data[2,],T2_penalty, type="null")
T2b_OptPart
#$changepoints
#[1]  5 10
#$means1
#[1] 0 1
#$means2
#[1] 0 1
#$globalCost
#[1] 0
PlotOptPart2D(T2b_data, T2b_OptPart$changepoints,T2b_OptPart$means1, T2b_OptPart$means2)

T2b_PELT <- OptPart2D(T2b_data[1,], T2b_data[2,], T2_penalty, type="pruning")
#$changepoints
#[1]  5 10
#$means1
#[1] 0 1
#$means2
#[1] 0 1
#$globalCost
#[1] 0
PlotOptPart2D(T2b_data, T2_PELT$changepoints,T2_PELT$means1, T2_PELT$means2)

####?????????????????
T2_l_penalty <- 0.001 # =>n segments
T2_l_pen_OptPart<-OptPart2D(T2b_data[1,], T2b_data[2,],T2_l_penalty, type="null")
T2_l_pen_OptPart
PlotOptPart2D(T2b_data, T2b_OptPart$changepoints,T2b_OptPart$means1, T2b_OptPart$means2)
T2_l_pen_PELT <- OptPart2D(T2b_data[1,], T2b_data[2,], T2_l_penalty, type="pruning")
T2_l_pen_PELT
PlotOptPart2D(T2b_data, T2_l_pen_PELT$changepoints,T2_l_pen_PELT$means1, T2_l_pen_PELT$means2)
####?????????????????

T2_b_penalty <- 10 # =>1 segment
T2_b_pen_OptPart<-OptPart2D(T2b_data[1,], T2b_data[2,],T2_b_penalty, type="null")
#$changepoints
#[1] 10
#$means1
#[1] 0.5
#$means2
#[1] 0.5
#$globalCost
#[1] 5
PlotOptPart2D(T2b_data, T2_b_pen_OptPart$changepoints,T2_b_pen_OptPart$means1, T2_b_pen_OptPart$means2)

T2_b_pen_PELT <- OptPart2D(T2_data[1,], T2b_data[2,], T2_b_penalty, type="pruning")
#$changepoints
#[1] 10
#$means1
#[1] 0.5
#$means2
#[1] 0.5
#$globalCost
#[1] 5
PlotOptPart2D(T2b_data, T2_b_pen_PELT$changepoints,T2_b_pen_PELT$means1, T2_b_pen_PELT$means2)


#Test3:The data has multiple segments (sigma = 1).
T3_n <- 50
T3_chp <- seq(from = 10, to = T3_n, by = 10)
T3_mu1 <- c(1, 5, 2, 0, 8)
T3_mu2 <- c(0, 10, 0, 7, 0)
T3_sigma <- 1
T3_penalty <- 2 * T3_sigma * log(T3_n)

T3_data <- GenData2D(T3_n, T3_chp, T3_mu1,T3_mu2,T3_sigma,T3_sigma)
T3_data

T3_OptPart<-OptPart2D(T3_data[1,], T3_data[2,],T3_penalty, type="null")
T3_OptPart
#$changepoints
#[1] 10 20 23 30 40 50
#$means1
#[1] 0.7713192 5.4919037 1.3170599 1.8332471 0.4243932 8.2982903
#$means2
#[1] -0.3322999  9.9201272 -1.5953358  0.4486696  6.8441019  0.6043954
#$globalCost
#[1] 121.7458
PlotOptPart2D(T3_data, T3_OptPart$changepoints,T3_OptPart$means1, T3_OptPart$means2)

T3_PELT <- OptPart2D(T3_data[1,], T3_data[2,], T3_penalty, type="pruning")
T3_PELT
#$changepoints
#[1] 10 20 23 30 40 50
#$means1
#[1] 0.7713192 5.4919037 1.3170599 1.8332471 0.4243932 8.2982903
#$means2
#[1] -0.3322999  9.9201272 -1.5953358  0.4486696  6.8441019  0.6043954

#$globalCost
#[1] 121.7458
PlotOptPart2D(T3_data, T3_PELT$changepoints,T3_PELT$means1, T3_PELT$means2)

#Test4: Data1 and data2 have different number of segments (sigma = 1).
T4_n <- 50
T4_chp <- seq(from = 10, to = T4_n, by = 10)
T4_mu1 <- c(5, 0, 8, 2, 10)
T4_mu2 <- c(0, 0, 5, 1, 0)
T4_sigma <- 1
T4_penalty <- 2 * T4_sigma * log(T4_n)

T4_data <- GenData2D(T4_n, T4_chp, T4_mu1,T4_mu2,T4_sigma,T4_sigma)

T4_OptPart<-OptPart2D(T4_data[1,], T4_data[2,],T4_penalty, type="null")
T4_OptPart
#$changepoints
#[1] 10 20 30 40 50
#$means1
#[1]  4.7387808 -0.3621536  7.9606156  1.8714787  9.8906521
#$means2
#[1] 0.3897931 0.3599870 4.7388699 1.5577089 0.0366370
#$globalCost
#[1] 130.3265
PlotOptPart2D(T4_data, T4_OptPart$changepoints,T4_OptPart$means1, T4_OptPart$means2)

T4_PELT <- OptPart2D(T4_data[1,], T4_data[2,], T4_penalty, type="pruning")
T4_PELT
#$changepoints
#[1] 10 20 30 40 50
#$means1
#[1]  4.7387808 -0.3621536  7.9606156  1.8714787  9.8906521
#$means2
#[1] 0.3897931 0.3599870 4.7388699 1.5577089 0.0366370
#$globalCost
#[1] 130.3265
PlotOptPart2D(T4_data, T4_PELT$changepoints,T4_PELT$means1, T4_PELT$means2)

