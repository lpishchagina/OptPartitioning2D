#include "Cost2D.h"
#include "OP2D.h"

#include<iostream>
#include <stdlib.h>

//constructor------------------------------------------------------------------//
OP2D::OP2D(std::vector< double >& data1, std::vector< double >& data2,double beta)
{
  penalty = beta;
  ndata = data1.size();
  
  vectK = new double*[ndata+1]; 
  for(unsigned int i = 0; i < ndata+1; i++)
  {
    vectK[i] = new double[3];
  }
  
  Q = new double [ndata+1];
  Q[0] = -penalty;
  Q[1] = 0;
  
  lastChangepoints = new int[ndata+1];
  lastChangepoints[0] = -1;
  lastChangepoints[1] = 0;
}
//destructor-------------------------------------------------------------------//
OP2D::~OP2D()
{
  delete []lastChangepoints;
  lastChangepoints = NULL;
  
  delete [] Q;
  Q = NULL;
  
  for(unsigned int i = 0; i < ndata+1; i++) {delete(vectK[i]);}
  delete [] vectK;
  vectK = NULL;
}
//accessory--------------------------------------------------------------------//
std::vector< int > OP2D::getChangepoints() const { return(changepoints); }
std::vector< double > OP2D::getMeans1() const { return(means1); }
std::vector< double > OP2D::getMeans2() const { return(means2); }
double OP2D::getGlobalCost() const { return(globalCost); }
unsigned int OP2D::getN() const  { return(ndata); }

//vectK is vector of Sum(y1t_y1T), Sum(y2t_y2T), Sum(y1t^2+y2t^2_y1T^2+y2T^2)--//
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
//Optimal Partitioning algorithm-----------------------------------------------//
void OP2D::algoOptPart(std::vector< double >& data1,std::vector< double >& data2)
{
  unsigned int n = data1.size();
  Cost2D Cost;
  double TempCost = 0;
  double TempQ = 0;
  int ChangePointTemp = -1;
  unsigned int a = 0;
  
  vectK = vectSum(data1,data2);
  
  //Optimal Partitioning//
  for ( unsigned int T = 2; T < n + 1 ; T++)
  {
    TempQ = INFINITY;
    for (unsigned int t = 0; t < T; t++)
    {
      a = t + 1;
      TempCost = Q[t] + Cost.Cost_ab(a, T, vectK[t], vectK[T]) + penalty;
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

//PELT algorithm---------------------------------------------------------------//
void OP2D::algoPELT(std::vector< double >& data1,std::vector< double >& data2)
{
  unsigned int n = data1.size();
  Cost2D Cost;
  double TempCost = 0;
  double TempQ = 0;
  unsigned int a = 0;
  int ChangePointTemp = -1;
  std::list<  unsigned int > SetR ={0};// Set of candidates for the best changepoint for (Y0,YT)
  std::list< unsigned int > ::iterator itSetR; 
  vectK = vectSum(data1, data2);
  //PELT//
  for ( unsigned int T = 2; T < n + 1; T++)
  {
    TempQ = INFINITY;
    ChangePointTemp = 0;
    itSetR = SetR.begin();
    while( itSetR != SetR.end() )
    {
      a = *itSetR + 1;
      TempCost = Q[*itSetR] + Cost.Cost_ab(a, T, vectK[*itSetR], vectK[T]) + penalty; 
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
      a = *itSetR+1;
      if( Q[*itSetR] + Cost.Cost_ab(a, T, vectK[*itSetR], vectK[T]) > Q[T]  ) 
      {
        itSetR = SetR.erase(itSetR);
      }
      else {++itSetR;}
    }
    SetR.push_back(T);
  }
}

//backtracking-----------------------------------------------------------------//
//a vector of change points and a vector of mean values
void OP2D::backtracking(int n)
{
  globalCost = Q[n];
  int ChangepointTemp = n;
  double mean1tT;
  double mean2tT;
  while (ChangepointTemp > 0)
  {
    changepoints.push_back(ChangepointTemp);
    mean1tT = (vectK[ChangepointTemp][0] - vectK[lastChangepoints[ChangepointTemp]][0]) / (ChangepointTemp - lastChangepoints[ChangepointTemp]);
    mean2tT = (vectK[ChangepointTemp][1] - vectK[lastChangepoints[ChangepointTemp]][1]) / (ChangepointTemp - lastChangepoints[ChangepointTemp]);
    means1.push_back(mean1tT);
    means2.push_back(mean2tT);
    ChangepointTemp = lastChangepoints[ChangepointTemp];
  }
  reverse(changepoints.begin(), changepoints.end());
  reverse(means1.begin(), means1.end());
  reverse(means2.begin(), means2.end());
}