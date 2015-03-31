/* 
 * This file contains NTI-functions
 */

// Rational function (see More than a meal)
double ratfun(double *ab,  // abundance of nt interactor
              double *ab0, // typical abundance of nt interactor
              double *p0,  // parameter, trophic     
              double *pnt) // parameter, non-trophic 
{ 
  
  return ( *pnt * *ab + *p0 * *ab0 ) / ( *ab + *ab0 ); 
  
}
