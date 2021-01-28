#Time complexity. Optimal Partitioning and PELT algorithms

devtools::install_github("lpishchagina/OptPartitioning2D")
library(OptPartitioning2D)



#Function OneStep  returns the execution time of a given algorithm
OneStep <- function(data1, data2, penalty, type, func = "OptPart2D")
{
  if(type == "null"){t <- system.time(OptPart2D(data1, data2, penalty, type = "null"))[[1]]}
  if(type == "pruning"){t <- system.time(OptPart2D(data1, data2, penalty, type = "pruning"))[[1]]}
  return(t)
}

#Test5: One time complexity test
library(microbenchmark)
library("ggplot2")

T5_n <- 5000
T5_chp <- seq(from = 100, to = T5_n, by = 100)
T5_chp
T5_mu1 <- rpois(T5_n/100, 10)
T5_mu1
T5_mu2 <- rpois(T5_n/100, 5)
T5_mu2
T5_sigma <- 1
T5_penalty <- 2 * T5_sigma * log(T5_n)

T5_data <- GenData2D(T5_n, T5_chp, T5_mu1, T5_mu2, T5_sigma, T5_sigma)

T5_timeOptPart <- OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D")
T5_timePELT <- OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D")
T5_timeOptPart
#[1]0.16
T5_timePELT
#[1] 0.03
T5_timeOptPart/T5_timePELT
#[1] 5.333333

##Test6: Iterations ( T5_data )

T6_nStep <- 10
T6_timeOptPart <- 0
T6_timePELT <- 0

for(i in 1:T6_nStep){T6_timeOptPart <- T6_timeOptPart + OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D")}
for(i in 1:T6_nStep){T6_timePELT <- T6_timePELT + OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D")}

T6_timeOptPart/T6_timePELT
#[1] 9.25

##Test7: microbenchmark ( T5_data, T7_data )
library(microbenchmark)
library("ggplot2")

T7_n <- 10000
T7_chp <- seq(from = 100, to = T7_n, by = 100)
T7_chp
T7_mu1 <- rpois(T7_n/100, 10)
T7_mu1
T7_mu2 <- rpois(T7_n/100, 5)
T7_mu2
T7_sigma <- 1
T7_penalty <- 2 * T7_sigma * log(T7_n)

T7_data <- GenData2D(T7_n, T7_chp, T7_mu1, T7_mu2, T7_sigma, T7_sigma)

# T5_n = 5000
T7_resT5_n <- microbenchmark( OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D"), OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D"),  times = 50)
T7_resT5_n
#Unit: milliseconds
#                                                                                      expr
#    OneStep(T5_data[1, ], T5_data[2, ], T5_penalty, type = "null",      func = "OptPart2D")
# OneStep(T5_data[1, ], T5_data[2, ], T5_penalty, type = "pruning",      func = "OptPart2D")
#      min       lq     mean   median       uq      max neval
# 258.2255 273.5639 351.7033 298.9834 373.9682 623.9380    50
# 112.8713 128.7546 158.5457 141.4346 203.9433 224.7527    50
autoplot(T7_resT5_n)


# T7_n = 10000
T7_resT7_n <- microbenchmark( OneStep(T7_data[1,], T7_data[2,], T7_penalty, type = "null",func = "OptPart2D"), OneStep(T7_data[1,], T7_data[2,], T7_penalty, type = "pruning",func = "OptPart2D"),  times = 50)
T7_resT7_n
Unit: milliseconds
#                                                                                       expr       min
#    OneStep(T7_data[1, ], T7_data[2, ], T7_penalty, type = "null",      func = "OptPart2D") 1378.7331
# OneStep(T7_data[1, ], T7_data[2, ], T7_penalty, type = "pruning",      func = "OptPart2D")  179.5884
#       lq     mean    median        uq       max neval
# 1518.303 2099.470 1672.9987 2099.0085 6110.8055    50
#  202.246  234.018  221.5641  260.0976  337.5985    50
autoplot(T7_resT7_n)

#estimate the difference  in running time

##Test8: Time complexity (the plot of the mean running time with respect to data length).
#Run T8_nRep times Optimal Partitioning and PELT algorithms of each value of the vector_n vector of length T8_nStep. We show the plot of the mean running time with respect to data length.
#
#

T8_nStep <- 10  
T8_vect_n <- seq(from = 1000, to = 10000, length.out = T8_nStep)
T8_vect_n 
#[1]  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000
T8_nRep <- 10

T8_resOptPart <- data.frame(matrix(0, T8_nStep, T8_nRep + 1))
colnames(T8_resOptPart) <- c("n", paste0("Rep",1:T8_nRep))

T8_resPELT <- data.frame(matrix(0, T8_nStep, T8_nRep + 1))
colnames(T8_resPELT) <- c("n", paste0("Rep",1:T8_nRep))

T8_sigma <- 1

for(i in 1:length(T8_vect_n))
{
  T8_chp <- seq(from = 100, to = T8_vect_n[i], by = 100)
  T8_mu1 <- rpois(T8_vect_n[i]/100, 10)
  T8_mu2 <- rpois(T8_vect_n[i]/100, 5)
  T8_penalty <- 2 * T8_sigma * log(T8_vect_n[i])
  
  T8_data <- GenData2D(T8_vect_n[i], T8_chp, T8_mu1, T8_mu2, T8_sigma, T8_sigma)
  
  T8_resOptPart[i,] <- c(T8_vect_n[i], replicate(T8_nRep, OneStep(T8_data[1,], T8_data[2,], T8_penalty, type = "null",func = "OptPart2D")))
  T8_resPELT[i,] <- c(T8_vect_n[i], replicate(T8_nRep, OneStep(T8_data[1,], T8_data[2,], T8_penalty, type = "pruning",func = "OptPart2D")))
}
T8_resOptPart
#       n Rep1 Rep2 Rep3 Rep4 Rep5 Rep6 Rep7 Rep8 Rep9 Rep10
#1   1000 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
#2   2000 0.07 0.03 0.04 0.03 0.01 0.03 0.03 0.03 0.04  0.04
#3   3000 0.05 0.03 0.03 0.04 0.06 0.03 0.03 0.05 0.06  0.05
#4   4000 0.11 0.09 0.10 0.07 0.09 0.11 0.11 0.11 0.11  0.11
#5   5000 0.16 0.19 0.18 0.16 0.20 0.14 0.19 0.14 0.16  0.16
#6   6000 0.23 0.22 0.20 0.25 0.18 0.25 0.31 0.30 0.25  0.28
#7   7000 0.37 0.45 0.47 0.44 0.49 0.42 0.45 0.45 0.53  0.49
#8   8000 0.78 0.79 0.79 0.73 0.78 0.86 0.73 0.83 0.82  0.77
#9   9000 0.99 0.97 1.02 1.00 0.98 1.04 1.09 1.47 1.18  1.09
#10 10000 1.55 1.65 1.51 1.75 1.44 1.46 1.45 1.50 1.39  1.49
T8_resPELT
#       n Rep1 Rep2 Rep3 Rep4 Rep5 Rep6 Rep7 Rep8 Rep9 Rep10
#1   1000 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
#2   2000 0.00 0.01 0.02 0.00 0.00 0.01 0.00 0.00 0.01  0.00
#3   3000 0.02 0.00 0.00 0.02 0.00 0.00 0.00 0.02 0.01  0.01
#4   4000 0.01 0.01 0.00 0.00 0.01 0.00 0.01 0.04 0.00  0.02
#5   5000 0.02 0.02 0.02 0.03 0.00 0.03 0.03 0.02 0.03  0.01
#6   6000 0.03 0.01 0.05 0.00 0.03 0.01 0.05 0.03 0.05  0.03
#7   7000 0.00 0.01 0.05 0.04 0.03 0.04 0.03 0.06 0.01  0.03
#8   8000 0.06 0.06 0.03 0.03 0.01 0.04 0.06 0.06 0.05  0.01
#9   9000 0.05 0.08 0.06 0.04 0.06 0.03 0.03 0.06 0.08  0.09
#10 10000 0.06 0.08 0.08 0.04 0.03 0.09 0.07 0.06 0.05  0.07

T8_mean_OptPart <- rowMeans(T8_resOptPart[,-1])
#[1] 0.000 0.035 0.043 0.101 0.168 0.247 0.456 0.788 1.083 1.519
plot(T8_vect_n, T8_mean_OptPart, xlab = "data length", ylab = "mean time in second",  main = "time complexity of Optimal Partitioning",  col="red3")
lines(T8_vect_n, T8_mean_OptPart,  col="red3")

T8_mean_PELT <- rowMeans(T8_resPELT[,-1])
#[1] 0.000 0.005 0.008 0.010 0.021 0.029 0.030 0.041 0.058 0.063
plot(T8_vect_n, T8_mean_PELT, xlab = "data length", ylab = "mean time in second",  main = "time complexity of PELT", col="steelblue")
lines(T8_vect_n, T8_mean_PELT, col="steelblue")

plot(T8_vect_n, T8_mean_OptPart, xlab = "data length", ylab = "mean time in second",  main = "time complexity of Optimal Partitioning and PELT", col="red3")
lines(T8_vect_n, T8_mean_OptPart, col="red3")
points(T8_vect_n, T8_mean_PELT, col="steelblue")
lines(T8_vect_n, T8_mean_PELT, col="steelblue")

location = "topleft"
labels = c("Optimal Partitioning", "PELT")
colors = c("red3", "steelblue")
legend(location, labels, fill=colors)

#Test9
T9_nStep <- 10  
T9_vect_n <- seq(from = 1000, to = 10000, length.out = T9_nStep)
T9_vect_n 
#[1]  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000

T9_vect_m <- floor(sqrt(T9_vect_n)/4) 
# [1]  7 11 13 15 17 19 20 22 23 25
T9_nRep <- 10

T9_resOptPart <- data.frame(matrix(0, T9_nStep, T9_nRep + 1))
colnames(T9_resOptPart) <- c("n", paste0("Rep",1:T9_nRep))

T9_resPELT <- data.frame(matrix(0, T9_nStep, T9_nRep + 1))
colnames(T9_resPELT) <- c("n", paste0("Rep",1:T9_nRep))

T9_sigma <- 1

for(i in 1:length(T9_vect_n))
{
	
  T9_chp <- c(sort(runif(T9_vect_m[i] - 1,0,T9_vect_n[i]-1)), T9_vect_n[i])
  T9_mu1 <- rpois(T9_vect_m[i], 10)
  T9_mu2 <- rpois(T9_vect_m[i], 5)
  T9_penalty <- 2 * T9_sigma * log(T9_vect_n[i])
  
  T9_data <- GenData2D(T9_vect_n[i], T9_chp, T9_mu1, T9_mu2, T9_sigma, T9_sigma)
  
  T9_resOptPart[i,] <- c(T9_vect_n[i], replicate(T9_nRep, OneStep(T9_data[1,], T9_data[2,], T9_penalty, type = "null",func = "OptPart2D")))
  T9_resPELT[i,] <- c(T9_vect_n[i], replicate(T9_nRep, OneStep(T9_data[1,], T9_data[2,], T9_penalty, type = "pruning",func = "OptPart2D")))
}
T9_resOptPart
#       n Rep1 Rep2 Rep3 Rep4 Rep5 Rep6 Rep7 Rep8 Rep9 Rep10
#1   1000 0.02 0.00 0.00 0.00 0.00 0.01 0.00 0.02 0.00  0.00
#2   2000 0.05 0.05 0.04 0.05 0.05 0.03 0.03 0.03 0.03  0.01
#3   3000 0.05 0.06 0.05 0.03 0.06 0.04 0.05 0.05 0.06  0.08
#4   4000 0.13 0.13 0.12 0.10 0.11 0.08 0.10 0.09 0.10  0.09
#5   5000 0.20 0.20 0.22 0.24 0.18 0.21 0.18 0.18 0.16  0.16
#6   6000 0.29 0.29 0.30 0.32 0.31 0.28 0.26 0.25 0.25  0.28
#7   7000 0.49 0.39 0.36 0.32 0.36 0.39 0.47 0.43 0.52  0.46
#8   8000 0.98 0.84 0.81 0.82 0.94 0.97 0.90 0.86 0.98  0.88
#9   9000 1.12 1.22 1.19 1.56 1.20 1.17 1.14 1.21 1.26  1.19
#10 10000 1.65 1.57 1.54 1.78 1.67 1.59 1.61 1.56 1.58  1.72
T9_resPELT
#       n Rep1 Rep2 Rep3 Rep4 Rep5 Rep6 Rep7 Rep8 Rep9 Rep10
#1   1000 0.00 0.01 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
#2   2000 0.02 0.01 0.00 0.00 0.00 0.01 0.00 0.01 0.00  0.01
#3   3000 0.00 0.04 0.02 0.02 0.03 0.03 0.01 0.02 0.00  0.01
#4   4000 0.02 0.02 0.02 0.01 0.03 0.01 0.01 0.03 0.03  0.02
#5   5000 0.04 0.05 0.05 0.03 0.04 0.03 0.03 0.03 0.04  0.01
#6   6000 0.06 0.07 0.07 0.04 0.06 0.07 0.05 0.06 0.10  0.07
#7   7000 0.05 0.05 0.05 0.07 0.08 0.06 0.09 0.08 0.11  0.04
#8   8000 0.17 0.20 0.16 0.10 0.16 0.15 0.12 0.15 0.14  0.14
#9   9000 0.09 0.11 0.11 0.13 0.08 0.06 0.11 0.12 0.12  0.14
#10 10000 0.14 0.13 0.16 0.09 0.11 0.17 0.09 0.15 0.16  0.13

T9_mean_OptPart <- rowMeans(T9_resOptPart[,-1])
T9_mean_OptPart
# [1] 0.005 0.037 0.053 0.105 0.193 0.283 0.419 0.898 1.226 1.627
plot(T9_vect_n, T9_mean_OptPart, xlab = "data length", ylab = "mean time in second",  main = "Time complexity of Optimal Partitioning",  col="red3")
lines(T9_vect_n, T9_mean_OptPart,  col="red3")

T9_mean_PELT <- rowMeans(T9_resPELT[,-1])
T9_mean_PELT 
# [1] 0.001 0.006 0.018 0.020 0.035 0.065 0.068 0.149 0.107 0.133
plot(T9_vect_n, T9_mean_PELT, xlab = "data length", ylab = "mean time in second",  main = "Time complexity of PELT", col="steelblue")
lines(T9_vect_n, T9_mean_PELT, col="steelblue")

plot(T9_vect_n, T9_mean_OptPart, xlab = "data length", ylab = "mean time in second",  main = "Time complexity of Optimal Partitioning and PELT", col="red3")
lines(T9_vect_n, T9_mean_OptPart, col="red3")
points(T9_vect_n, T9_mean_PELT, col="steelblue")
lines(T9_vect_n, T9_mean_PELT, col="steelblue")

location = "topleft"
labels = c("Optimal Partitioning", "PELT")
colors = c("red3", "steelblue")
legend(location, labels, fill=colors)



