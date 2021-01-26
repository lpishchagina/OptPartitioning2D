#include "Cost2D.h"
#include "OP2D.h"

#include<iostream>
#include <stdlib.h>

//constructor------------------------------------------------------------------//
OP2D::OP2D(std::vector< double >& data1, std::vector< double >& data2,double beta)
{
  penalty = beta;
  ndata = data1.size();
  
  /*vectData1 = new double[ndata];
  vectData2 = new double[ndata];
  for(unsigned int i = 0; i < ndata; i++)
  {
    vectData1[i] = data1[i];
    vectData2[i] = data2[i];
  }
  */
  vectK = new double*[ndata+1]; 
  for(unsigned int i = 0; i < ndata+1; i++)
  {
    vectK[i] = new double[ndata + 1];
  }
  
  Q = new double [ndata+1];
  lastChangepoints = new unsigned int[ndata+1];
  
  Q[0] = -penalty;
  Q[1] = 0;
  lastChangepoints[0] = -1;
  lastChangepoints[1] = 0;
}
//destructor-------------------------------------------------------------------//
OP2D::~OP2D()
{
 /* delete []vectData1;
  vectData1 = NULL;
  
  delete []vectData2;
  vectData2 = NULL;
  */
  delete []lastChangepoints;
  lastChangepoints = NULL;
  
  delete [] Q;
  Q = NULL;
  
  for(unsigned int i = 0; i < ndata+1; i++) {delete(vectK[i]);}
  delete [] vectK;
  vectK = NULL;
}
//accessors--------------------------------------------------------------------//
std::vector< unsigned int > OP2D::getChangepoints() const { return(changepoints); }
std::vector< double > OP2D::getMeans1() const { return(means1); }
std::vector< double > OP2D::getMeans2() const { return(means2); }
double OP2D::getGlobalCost() const { return(globalCost); }
unsigned int OP2D::getN() const  { return(ndata); }
//vectSum----------------------------------------------------------------------//
double** OP2D::vectSum(std::vector< double >& data1, std::vector< double >& data2) const
{
  unsigned int n = data1.size();
  vectK[0][0] = 0;
  vectK[0][1] = 0;
  vectK[0][2] = 0;
  for (unsigned int j = 1; j < (n + 1); j++)
  {
    vectK[j][0] = vectK[j-1][0] + data1[j-1];
    vectK[j][1] = vectK[j-1][1] + data2[j-1];
    vectK[j][2] = vectK[j-1][2] + data1[j-1] * data1[j-1] + data2[j-1] * data2[j-1];
  }
  return(vectK);
}
//OptPart----------------------------------------------------------------------//
void OP2D::algoOptPart(std::vector< double >& data1,std::vector< double >& data2)
{
  unsigned int n = data1.size();
  Cost2D Cost;
  double TempCost = 0;
  double TempQ = 0;
  int ChangePointTemp = -1;
  
  vectK = vectSum(data1,data2);
  
  //Optimal Partitioning
  for (unsigned int T = 2; T < (n + 1); T++)
  {
    TempQ = INFINITY;
    for (unsigned int t = 0; t < T; t++)
    {
      TempCost = Q[t] + Cost.Cost_tT(t, T, vectK[t], vectK[T]) + penalty;// vectK[t-1] => vectK[t]
      if (TempQ > TempCost)
      {
        TempQ = TempCost;
        ChangePointTemp = t;
      }
    }
    Q[T] = TempQ;
    lastChangepoints[T] = ChangePointTemp; //best last changepoint for (Y0,YT)
  }  
}
//PELT-------------------------------------------------------------------------//
void OP2D::algoPELT(std::vector< double >& data1,std::vector< double >& data2)
{
  
  unsigned int n = data1.size();
  Cost2D Cost;
  double TempCost = 0;
  double TempQ = 0;
  int ChangePointTemp = -1;
  std::list< unsigned int > SetR ={0};// Set of candidates for the best changepoint for (Y0,YT)
  std::list< unsigned int > ::iterator itSetR; 
  vectK = vectSum(data1, data2); /// vectors Sum y1,y2,y1^2+y2^2
  //PELT//
  for (unsigned int T = 2; T < (n + 1); T++)
  {
    TempQ = INFINITY;
    ChangePointTemp = 0;
    itSetR = SetR.begin();
    while( itSetR != SetR.end() )
    {
      TempCost = Q[*itSetR] + Cost.Cost_tT(*itSetR, T, vectK[*itSetR], vectK[T]) + penalty; //vectK[*itSetR-1] =>vectK[*itSetR]
      if (TempQ > TempCost)
      {
        TempQ = TempCost;
        ChangePointTemp = *itSetR;
      }
      ++itSetR;
    }
    Q[T] = TempQ;
    lastChangepoints[T] = ChangePointTemp;
    //pruning
    itSetR = SetR.begin();
    while( itSetR != SetR.end() )
    {
      if( Q[*itSetR] + Cost.Cost_tT(*itSetR+1, T, vectK[*itSetR+1], vectK[T]) > Q[T]  ) // vectK[*itSetR]=> vectK[*itSetR+1],*itSetR=> *itSetR+1
      {
        SetR.assign(1,T);
      }
      ++itSetR;
    }
    SetR.push_back(T);
    SetR.unique();
  }
}
//backtracking-----------------------------------------------------------------//
//a vector of change points and a vector of mean values
void OP2D::backtracking(unsigned int n)
{
  globalCost = Q[n];
  unsigned int ChangepointTemp = n;
  double mean1tT;
  double mean2tT;
  while (ChangepointTemp > 1)
  {
    changepoints.push_back(ChangepointTemp);
    mean1tT = (vectK[ChangepointTemp][0] - vectK[lastChangepoints[ChangepointTemp] - 1][0]) / (ChangepointTemp - lastChangepoints[ChangepointTemp] + 1);
    mean2tT = (vectK[ChangepointTemp][1] - vectK[lastChangepoints[ChangepointTemp] - 1][1]) / (ChangepointTemp - lastChangepoints[ChangepointTemp] + 1);
    means1.push_back(mean1tT);
    means2.push_back(mean2tT);
    ChangepointTemp = lastChangepoints[ChangepointTemp];
  }
}


