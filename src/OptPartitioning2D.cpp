#include<Rcpp.h>
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

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
//' OptPart2D(data1 = c(0,1,0,8,5), data2 = c(0,1,0,1,1), penalty = 2,  type = "null")                                                                                      
//' OptPart2D(data1 = c(0,1,0,8,5), data2 = c(0,1,0,1,1), penalty = 2,  type = "pruning")

// [[Rcpp::export]]
List OptPart2D(std::vector<double> data1, std::vector<double> data2, double penalty, std::string type) {
  if(data1.size() != data2.size()){throw std::range_error("data1 and data2 have different length");}
  
  if(penalty <= 0) {throw std::range_error("Penalty should be a positive number");}
  
  if(type != "null" && type != "pruning")
  {throw std::range_error("type must be one of: null or pruning");}
  
  OP2D Y = OP2D(data1,data2, penalty);
  
  if(type == "null") {Y.algoOptPart(data1, data2);}
  if(type == "pruning") {Y.algoPELT(data1, data2);}
  
  Y.backtracking(Y.getN());
  
  List res = List::create( _["changepoints"] = Y.getChangepoints(),
                           _["means1"] = Y.getMeans1(),
                           _["means2"] = Y.getMeans2(),
                           _["globalCost"] = Y.getGlobalCost() ) ;
  
  return res ;
}
