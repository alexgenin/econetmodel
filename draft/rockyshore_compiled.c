#include <R.h>

// Print debug messages ?
static int dbg=0;

/*
 * Note: C STORES ARRAYS AS ROW-MAJOR !!!
*/

#define Np 2  // equal to neq in the function
static int npars=10; 

union p { 
  struct {  
    double r[Np];
    double K[Np];
    double w[Np][Np];
    double e[Np][Np];
    double h[Np];
    double x[Np];
    double atk[Np][Np];
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
  int N=npars;
  odeparms(&N, p.value);
}

/* Derivatives and 1 output variable */
void rockyshore(int *neq,     // neq is the number of equations
                double *t,    // value of the independent variable (time)
                double *y,    // current state
                double *ydot, // will contain the derivatives
                double *yout, // vector with [nout values of other output variables] 
                              // ++ [rpar values]
                int *ip)      // [number of output vals, 
                          //  length of yout,
                          //  length of *ip]
{
  
  // Ensures correct memory allocation
  if (ip[0] < 1) error("nout should be at least 1");
  
  for (int i=0; i<*neq; i++) { 
    ydot[i] = 0;
  }
  
  // Compute logistic growth for each species
  // NB: we assign and initialize the ydot values here
  for (int i=0; i<*neq; i++) { 
    ydot[i] = p.r[i] * y[i] * (1 - y[i] / p.K[i]);
  }
  
  // Compute interactions
  for (int i=0; i<*neq; i++) {
    for (int j=0; j<*neq; j++) {
      
      if (dbg) { 
        Rprintf("%i %f |", i, ydot[i]);
      }
      
      // Species i eats
      ydot[i] += y[i] * p.w[i][j] * p.atk[i][j] * y[j] / 
                   (1 + p.atk[i][j] * p.h[j] * y[j]);
      
      // Species i gets eaten
      ydot[i] += - y[j] * p.w[j][i] * p.atk[j][i] * y[i] / 
                     (1 + p.atk[j][i] * p.h[i] * y[i]);
      
    }
  }
  
  // Mortality
  for (int i=0; i<*neq; i++) { 
    ydot[i] += - p.xs[i] * y[i];
  }
  
  if (dbg) { 
    Rprintf("\n");
  }
  
  // Compute other stuff we want to return
  // yout[0] = 2;
  // yout[1] = 3;
  
  // NB: there is no need to return anything: all the stuff is modified in place
  // and [ydot]++[yout] is returned
}

// See ./draft/rockyshore_compiled.R for a test case
