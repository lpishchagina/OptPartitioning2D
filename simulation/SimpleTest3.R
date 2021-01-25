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
T5_n <- 1000
T5_chp <- seq(from = 100, to = T5_n, by = 100)
T5_chp
T5_mu1 <- rpois(T5_n/100, 10)
T5_mu1
T5_mu2 <- rpois(T5_n/100, 5)
T5_mu2
T5_sigma <- 1
T5_penalty <- 2 * T5_sigma * log(T5_n)

T5_data <- GenData2D(T5_chp, T5_mu1, T5_mu2, T5_sigma, T5_sigma)

T5_timeOptPart <- OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D")
T5_timePELT <- OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D")
T5_timeOptPart
T5_timePELT
T5_timeOptPart/T5_timePELT

##Test6: Iterations ( T5_data )

T6_nStep <- 10
T6_timeOptPart <- 0
T6_timePELT <- 0

for(i in 1:T6_nStep){T6_timeOptPart <- T6_timeOptPart + OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D")}
for(i in 1:T6_nStep){T6_timePELT <- T6_timePELT + OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D")}

T6_timeOptPart/T6_timePELT

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

T7_data <- GenData2D(T7_chp, T7_mu1, T7_mu2, T7_sigma, T7_sigma)

# T5_n = 1000
T7_resT5_n <- microbenchmark( OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "null",func = "OptPart2D"), OneStep(T5_data[1,], T5_data[2,], T5_penalty, type = "pruning",func = "OptPart2D"),  times = 50)
autoplot(T7_resT5_n)
T7_resT5_n

# T7_n = 10000
T7_resT7_n <- microbenchmark( OneStep(T7_data[1,], T7_data[2,], T7_penalty, type = "null",func = "OptPart2D"), OneStep(T7_data[1,], T7_data[2,], T7_penalty, type = "pruning",func = "OptPart2D"),  times = 50)
autoplot(T7_resT7_n)
T7_resT7_n
#estimate the difference  in running time

##Test8: Time complexity (the plot of the mean running time with respect to data length).
#Run T8_nRep times Optimal Partitioning and PELT algorithms of each value of the vector_n vector of length T8_nStep. We show the plot of the mean running time with respect to data length.
#
#

T8_nStep <- 10 #20 
T8_vect_n <- seq(from = 10000, to = 100000, length.out = T8_nStep)
# T8_vect_n 
#[1] 1e+04 2e+04 3e+04 4e+04 5e+04 6e+04 7e+04 8e+04 9e+04 1e+05
T8_nRep <- 10 #50

T8_resOptPart <- data.frame(matrix(0, T8_nStep, T8_nRep + 1))
colnames(T8_resOptPart) <- c("n", paste0("Rep",1:T8_nRep))

T8_resPELT <- data.frame(matrix(0, T8_nStep, T8_nRep + 1))
colnames(T8_resPELT) <- c("n", paste0("Rep",1:T8_nRep))

j <- 1
T8_sigma <- 1

for(i in T8_vect_n)
{
  T8_chp <- seq(from = 100, to = T8_vect_n[i], by = 100)
  T8_mu1 <- rpois(T8_vect_n[i]/100, 10)
  T8_mu2 <- rpois(T8_vect_n[i]/100, 5)
  T8_penalty <- 2 * T8_sigma * log(T8_vect_n[i])
  
  T8_data <- GenData2D(T8_chp, T8_mu1, T8_mu2, T8_sigma, T8_sigma)
  
  T8_resOptPart[j,] <- c(i, replicate(T8_nRep, OneStep(T8_data[1,], T8_data[2,], T8_penalty, type = "null",func = "OptPart2D")))
  T8_resPELT[j,] <- c(i, replicate(T8_nRep, OneStep(T8_data[1,], T8_data[2,], T8_penalty, type = "pruning",func = "OptPart2D")))
  j <- j + 1
}

T8_mean_OptPart <- rowMeans(T8_resOptPart[,-1])
plot(T8_vect_n, T8_mean_OptPart, xlab = "data length", ylab = "mean time in second", main = "time complexity of Optimal Partitioning")

T8_mean_PELT <- rowMeans(T8_resOptPELT[,-1])
plot(T8_vect_n, T8_mean_PELT, xlab = "data length", ylab = "mean time in second",  main = "time complexity of PELT")




