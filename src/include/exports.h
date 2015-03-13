
// Gives access to the defined functional functional responses

#ifndef NETMODR_EXPORTS
#define NETMODR_EXPORTS

double frtype2(double *prey, 
               double *atk, 
               double *h);

int eq_reached(int *neq, 
               double *ydot, 
               double *zerotol);

double nti_ratfun(double *ab, 
                  double *ab0,
                  double *p0, 
                  double *pnt);

#endif
