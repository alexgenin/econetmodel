#include <R.h>
#include "./functional_response.h"

// Print debug messages ?
static int dbg=0;
static double zerotol=1e-30;

/*
 * Note: C STORES ARRAYS IN ROW-MAJOR ORDER !!!
*/

/* Template generation will produce two #define macros here for : 
 *   - Nsp : the number of species (equal to *neq in the solver function)
 *   - Np  : the number of parameters 
 */

// // Generated by R (Mon Feb 23 12:07:42 2015)
#define Nsp 5
#define Np 95
static int staticNp = 95;


/* We pass all our parameters as a big array of doubles. For convenience we 
 * define here a union/struct that allows referring to them as p.<name> so 
 * the code is much more readable. 
 * 
 * Same thing here, the tag will be replaced by the actual values of the 
 * parameters.
 */
union p { 
  struct {  
    // // Generated by R (Mon Feb 23 12:07:42 2015)
double r[Nsp];
double K[Nsp];
double e[Nsp][Nsp];
double w[Nsp][Nsp];
double h[Nsp];
double x[Nsp];
double atk[Nsp][Nsp];
  };
  double value[Np];
} p; // instance (name to be referred to)



/* So parameters are not visible to the model functions by default: this 
 * function ensures that they are correctly passed.
 * 
 * This function takes a single argument: a pointer to a function of two args
 * (see below).
 */
void initmod(void (* odeparms)(int *, double *)) {
  int N=Np;
  odeparms(&N, p.value); // odeparms allocates memory
}


/* This is the core function of the model */
void derivs(int *neq,     // neq is the number of equations
                double *t,    // value of the independent variable (time)
                double *y,    // current state
                double *ydot, // will contain the derivatives
                double *yout, // vector with [nout values of other output variables] 
                              // ++ [rpar values]
                int *ip)      // [number of output vals, 
                              //  length of yout,
                              //  length of *ip]
{
  double funcresp=0; // functional response
  
  // Ensures correct memory allocation
  if (ip[0] < 1) error("nout should be at least 1");
  
  for (int i=0; i<*neq; i++) { 
    if (ydot[i] <= zerotol) { 
      ydot[i] = 0;
    }
  }
  
  // Compute logistic growth for each species
  for (int i=0; i<*neq; i++) { 
    ydot[i] = p.r[i] * y[i] * (1 - y[i] / p.K[i]);
  }
  
  // Compute effect of trophic interactions
  for (int i=0; i<*neq; i++) {
    for (int j=0; j<*neq; j++) {
      
      if (dbg) { 
        if (i == 1 && j == 0) { 
          Rprintf("%i %i: %f |", i+1, j+1, 
                  frtype2(&y[j], &p.atk[i][j], &p.h[j]));
        }
      }
      
      // Species i eats j
      funcresp = frtype2(&y[j], &p.atk[i][j], &p.h[j]);
      ydot[i] += y[i] * p.w[i][j] * funcresp;
      
      // Species i gets eaten by j
      funcresp = frtype2(&y[i], &p.atk[j][i], &p.h[i]);
      ydot[i] += - y[j] * p.w[j][i] * funcresp / p.e[j][i];
      
    }
  }
  
  // Mortality
  for (int i=0; i<*neq; i++) { 
    ydot[i] += - p.x[i] * y[i];
  }
  
  if (dbg) { 
    Rprintf("\n");
  }
  
  // Compute other stuff we want to return if necessary
  for (int i=0; i<*neq; i++) { 
    yout[0] += y[i] + ydot[i]; // total abundance
  }
  
  // NB: there is no need to return anything: all the stuff is modified in place
  // and [ydot]++[yout] is returned
}

/* We define here an event that will happen at a time specified in R */
void spkill(int *n, double *t, double *y) { 
  // Modifies the system's state in place !
  y[4] = 0; // OMG kill a top predator !!!
}

