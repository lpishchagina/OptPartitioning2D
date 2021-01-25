#include<math.h>
#include "Cost2D.h"
#include "OP2D.h"

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List OptPatr2D(std::vector<double> data1, std::vector<double> data2, double penalty, std::string type) {
  OP2D Y = OP2D(data1,data2, penalty);
  
  if(type == "null") {Y.algoOptPart(data1, data2);}
  if(type == "pruning") {Y.algoPELT(data1, data2);}
  
  Y.backtracking(Y.getN());
  
  List res            = List::create( _["changepoints"] = Y.getChangepoints(),
                                      _["means1"] = Y.getMeans1(),
                                      _["means2"] = Y.getMeans2(),
                                      _["globalCost"] = Y.getGlobalCost() ) ;
  
  return res ;
}
