
// This file contains test functions to monitor the simulation run
#include <math.h>   // for fmaxf
#include <stdlib.h> // for abs

// Tests if an equilibrium is reached 
int eq_reached(int *neq,
               double *ydot, // pointer to the first element of array
               double *zerotol) { 
  
  // do not set value to 0 otherwise the solver will return an error 
  // (root detected on step 1)
  double max_deriv = -1; 
  int eq=0;
  
  // Get max derivative
  for (int i=0; i<*neq; i++) {
    max_deriv = fmaxf(max_deriv, fabs(ydot[i]));
  }
  
  if (max_deriv <= *zerotol) { 
    eq = 1;
  }
  
  return eq;
}
