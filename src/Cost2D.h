#ifndef COST2D_H
#define COST2D_H

#include <math.h>

class Cost2D
{
public:
  Cost2D();
  
  //value of the cost function on the interval [a, b]
  double Cost_ab(unsigned int& a, unsigned int& b, double* ka_1, double* kb);  
};

#endif // COST2D_H