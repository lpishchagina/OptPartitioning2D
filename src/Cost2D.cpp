#include"Cost2D.h"
#include <iostream>
#include <math.h>

Cost2D::Cost2D(){}

double Cost2D::Cost_ab(unsigned int& a, unsigned int& b,  double* ka_1, double* kb)
{
  double res = (-1)*((kb[0]-ka_1[0])*(kb[0]-ka_1[0]) + (kb[1]-ka_1[1])*(kb[1]-ka_1[1]))/(b - a + 1) + (kb[2] - ka_1[2]);
  return (res);
}