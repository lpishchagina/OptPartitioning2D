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
//' @param data1 is a vector of data1(a univariate time series).                                
//' @param data2 is a vector of data2(a univariate time series).                                
//' @param penalty is a value of penalty (a non-negative real number).                                        
//' @param type is a string defining the  algorithm ("null" = Optimal Partitioning, "pruning" = PELT).       
//'                                                                                                          
//' @return a list of 4 elements  = (changepoints, means1, means2, GlobalCost).                    
//'  
//' \describe{
//' \item{\code{changepoints}}{is the vector of changepoints.}
//' \item{\code{means1}}{is the vector of successive means for data1.}
//' \item{\code{means2}}{is the vector of successive means for data2.}
//' \item{\code{globalCost}}{is a number equal to the global cost.}
//' }                                                                                                                                                                             #     
//'             
//' @examples OptPart2D(data1 = c(0,0,0,1,1,1), data2 = c(2,2,2,0,0,0), penalty = 2*log(6),  type = "null") 
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
