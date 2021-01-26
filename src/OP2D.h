#ifndef OP2D_H
#define OP2D_H

#include"Cost2D.h"

#include <math.h>
#include<vector>
#include<list>
#include<iterator>

#include "Rcpp.h"

class OP2D
{
public:
  OP2D(std::vector< double >& data1, std::vector< double >& data2, double beta);
  ~OP2D();
  
  std::vector< unsigned int > getChangepoints() const;
  std::vector< double > getMeans1() const;
  std::vector< double > getMeans2() const;
  double getGlobalCost() const;
  unsigned int getN() const;
  
  double** vectSum(std::vector< double >& data1, std::vector< double >& data2) const;
  
  void algoOptPart(std::vector< double >& data1,std::vector< double >& data2);
  void algoPELT(std::vector< double >& data1, std::vector< double >& data2);
  void backtracking(unsigned int n);
  
private:
  double penalty;
  unsigned int ndata;
  
  double** vectK;
  //double* vectData1;
 // double* vectData2;
  
  unsigned int* lastChangepoints;
  double* Q;
  std::vector< unsigned int > changepoints; 
  std::vector< double > means1; 
  std::vector< double > means2;
  double globalCost;
};

#endif // OP2D_H