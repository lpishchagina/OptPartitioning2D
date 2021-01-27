#include<math.h>
#include "Cost2D.h"
#include "OP2D.h"


#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//' @title OptPart2D 
//'                                                                                                        
//' @description Optimal Partitioning and PELT algorithms for changepoints dimension 2                           
//'                                                                                                       
//' @param data1 vector of data1 to segment (a univariate time series).                                
//' @param data2 vector of data2 to segment (a univariate time series).                                
//' @param penalty value of penalty (a non-negative real number).                                        
//' @param type string defining the  algorithm ("null" = Optimal Partitioning, "pruning" = PELT).       
//'                                                                                                          
//' @return a list of 4 elements  = (changepoints, means1, means2, GlobalCost).                    
//'                                                                                                                                                                               #
//' @param changepoints vector of changepoints.                                                                   
//' @param means1 vector of successive means for data1.                                                           
//' @param means2 vector of successive means for data2.
//' @param globalCost value of global cost.       
//'             
//' @exemples 
//' data <- GenData2D (10, changepoints = c(2, 4, 6,8, 10), means1 = c(0, 1, 0, 1, 0), means2 = c(1, 2, 3, 4, 5), noise1 = 1, noise2 = 1)
//' resOptPart <- OptPart2D(data[1,], data[2,], penalty = 2*log(10),  type = "null")
//' resPELT <- OptPart2D(data[1,], data[2,], penalty = 2*log(10),  type = "pruning")

// [[Rcpp::export]]
List OptPart2D(std::vector<double> data1, std::vector<double> data2, double penalty, std::string type) {
  if(data1.size() != data2.size()){throw std::range_error("data1 and data2 have different length");}
  
  if(penalty < 0) {throw std::range_error("Penalty should be a non-negative number");}
  
  if(type != "null" && type != "pruning")
  {throw std::range_error("type must be one of: null or pruning");}
  
  OP2D Y = OP2D(data1,data2, penalty);
  
  if(type == "null") {Y.algoOptPart(data1, data2);}   //Optimal Partitioning algorithm
  if(type == "pruning") {Y.algoPELT(data1, data2);}   //PELT algorithm
  
  Y.backtracking(Y.getN());                           //algorithm results 
  
  List res;
  res["changepoints"] = Y.getChangepoints();
  res["means1"] = Y.getMeans1();
  res["means2"] = Y.getMeans2();
  res["globalCost"] = Y.getGlobalCost();
  
  
  return res;
}
