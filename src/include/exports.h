

#ifndef NETMODR_EXPORTS
#define NETMODR_EXPORTS

// Functional response(s)
double frhill(double *prey, 
<<<<<<< HEAD
              double *sumpreyatk, 
              double *atk, 
              double *w,
=======
              double *total_prey, 
              double *atk, 
              double *w, 
>>>>>>> master
              double *h,
              double *q);

// Safe pow() for negative numbers (see .c file)
double pow2(double x, 
            double y);

// Non-trophic responses
double nti_ratfun(double *ab, 
                  double *ab0,
                  double *p0, 
                  double *pnt);

// Control functions
int eq_reached(int *neq, 
               double *ydot, 
               double *zerotol);

#endif
