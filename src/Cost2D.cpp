#include"Cost2D.h"
#include <iostream>
#include <math.h>

Cost2D::Cost2D(){}

double Cost2D::Cost_tT(unsigned int& t, unsigned int& T,  double* kt_1, double* kT)
{
  double res = (-1)*((kT[0]-kt_1[0])*(kT[0]-kt_1[0]) + (kT[1]-kt_1[1])*(kT[1]-kt_1[1]))/(T - t + 1) + (kT[2] - kt_1[2]);
  return (res);
}