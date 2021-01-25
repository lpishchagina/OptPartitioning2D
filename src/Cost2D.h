#ifndef COST2D_H
#define COST2D_H

#include <math.h>

class Cost2D
{
public:
  Cost2D();
  double Cost_tT(unsigned int& t, unsigned int& T, double* kt_1, double* kT);
};

#endif // COST2D_H