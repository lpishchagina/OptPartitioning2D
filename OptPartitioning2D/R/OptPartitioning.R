
#' @title GenData2D
#'  
#' @description Generation of data of dimension 2 with a given values of means and changepoints
#' 
#' @param n number of data point.
#' @param changepoints a vector of increasing changepoint indices (last element is always n!).
#' @param means1 vector of successive means1 for data1.
#' @param means2 vector of successive means2 for data2.
#' @param noise1 standard deviation of an additional normal noise (for data1).
#' @param noise2 standard deviation of an additional normal noise (for data2).
#'  
#' @return matrix of data of dimension 2 x n with a given values of means by the segmentation.
#'  
#' @exemples
#' GenData2D <- function (n, changepoints, means1, means2, noise1 = 1, noise2 = 1)

 
GenData2D <- function(n, changepoints, means1, means2, noise1 = 1, noise2 = 1)
{
  #---stop---#
  if (n!= changepoints[length(changepoints)]) {stop('last element of changepoints is always n')}
  
  if(!is.numeric(changepoints)) {stop('changepoints are not all numeric')}
  if(is.unsorted(changepoints)) {stop('changepoints should be an increasing vector')}
  
  if(!is.numeric(means1)){stop('means1 are not all numeric')}
  if(!is.numeric(means2)){stop('means2 are not all numeric')}
  if( length(means1) != (length(means2)) ) {stop('means1 and means2 vectors are of different size')}
  if( length(changepoints) != length(means1) ) {stop('changepoints and means1 vectors are of different size')}
  if( length(changepoints) != length(means2) ) {stop('changepoints and means2 vectors are of different size')}
  
  if(!is.double(noise1)) {stop('noise1 is not a double')}
  if(noise1 < 0) {stop('noise1 must be non-negative')}
  if(!is.double(noise2)) {stop('noise2 is not a double')}
  if(noise2 < 0) {stop('noise2 must be non-negative')}
  #---function---#	
  data <- matrix(0,2,n)
  InttT<- diff(c(0,changepoints))
  # rnorm(mu,noise) = mu + rnorm(0,noise)
  data[1,] <- rep(means1, InttT) + rnorm(n, 0, noise1)
  data[2,] <- rep(means2, InttT) + rnorm(n, 0, noise2)
  return(data)
}

#' @title OptPatr2D 
#'                                                                                                        
#' @description Optimal Partitioning and PELT algorithms for changepoints dimension 2                           
#'                                                                                                       
#' @param data1 vector of data1 to segment (a univariate time series).                                
#' @param data2 vector of data2 to segment (a univariate time series).                                
#' @param penalty value of penalty (a non-negative real number).                                        
#' @param type string defining the  algorithm ("null" = Optimal Partitioning, "pruning" = PELT).       
#'                                                                                                          
#' @return a list of 4 elements  = (changepoints, means1, means2, GlobalCost).                    
#'                                                                                                                                                                               #
#' @param changepoints vector of changepoints.                                                                   
#' @param means1 vector of successive means for data1.                                                           
#' @param means2 vector of successive means for data2.
#' @param globalCost value of global cost.       
#'             
#' @exemples 
#' OptPatr2D(data1 = c(0,1,0,8,5), data2 = c(0,1,0,1,1), penalty = 2,  type = "null")                                                                                      
#' OptPatr2D(data1 = c(0,1,0,8,5), data2 = c(0,1,0,1,1), penalty = 2,  type = "pruning")


OptPatr2D <- function(data1, data2, penalty = 0, type = "null")
{
  #---stop---#
  if(!is.numeric(data1)){stop('data1 values are not all numeric')}
  if(!is.numeric(data2)){stop('data2 values are not all numeric')}
  if(length(data1) != length(data2)) {stop('data1 and data2 have different length')}
  
  if(!is.double(penalty)) {stop('penalty is not a double')}
  if(penalty < 0) {stop('penalty must be non-negative')}
  
  allowed.types <- c("null", "pruning")
  if(!type %in% allowed.types){stop('type must be one of: ', paste(allowed.types, collapse=", "))}
  
  #---CALL Rcpp function---#
  res <- TransferOptPatr2D( data1, data2, penalty, type) 
  response <- list(changepoints = res$changepoints, means1 = res$means1, means2 = res$means2, globalCost = res$globalCost)
  return(response)
}

#' @title PlotOptPart2D
#'  
#' @description Plot of data with a  values of means and changepoints.
#' 
#' @param data matrix of data of dimension 2 x n.
#' @param n number of data point.
#' 
#' @param changepoints a vector of increasing changepoint indices (last element is always n!).
#' @param means1 vector of successive means1 for data1.
#' @param means2 vector of successive means2 for data2.
 
#' @return 2 plot of data with a given values of means and changepoints.
#'  
#' @exemples 
#' data <- GenData2D (20, changepoints = c(1,5,15,20), means1 = c(0,10,0,10), means2 = c(1,2,3,4), noise1 = 1, noise2 = 1)
#' PlotOptPart2D (data, changepoints = c(1,5,15,20), means1 = c(0,10,0,10), means2 = c(1,2,3,4))

PlotOptPart2D <- function (data, changepoints, means1, means2)
{
  #---stop---#
  if (length(data[1,])!= changepoints[length(changepoints)]) {stop('last element of changepoints is always n')}
  
  if(!is.numeric(data)) {stop('data values are not all numeric')}
  if(is.unsorted(changepoints)) {stop('changepoints should be an increasing vector')}
  
  if(!is.numeric(means1)){stop('means1 are not all numeric')}
  if(!is.numeric(means2)){stop('means2 are not all numeric')}
  if( length(means1) != (length(means2)) ) {stop('means1 and means2 vectors are of different size')}
  if( length(changepoints) != length(means1) ) {stop('changepoints and means1 vectors are of different size')}
  if( length(changepoints) != length(means2) ) {stop('changepoints and means2 vectors are of different size')}
  
  #---function---#
  tau <- c(0, changepoints)
  par(mfrow = rbind(2,1))
  plot(c(1:length(data[1,])), data[1,], main = "Optimal Partitioning Y1", xlab = "time", ylab = "Y1", col = "blue")
  abline(v = changepoints, lty = 2, col = "grey")
  i<-1
  while(i != length(tau))
  {
    lines(c(tau[i],tau[i+1]), c(means1[i],means1[i]), type = "l", col = "red")
    i<-i+1
  }
  plot(c(1:length(data[2,])), data[2,], main = "Optimal Partitioning Y2 ", xlab = "time", ylab = "Y2", col = "red")
  abline(v = changepoints, lty = 2, col = "grey")
  i<-1
  while(i != length(tau))
  {
    lines(c(tau[i],tau[i+1]), c(means2[i],means2[i]), type = "l", col = "blue")
    i<-i+1
  }
}
