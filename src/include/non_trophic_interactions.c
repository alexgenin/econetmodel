/* 
 * This file contains NTI-describing functions
 */

// Summariser
// double nti_sum(double *ab[Nsp], 
//                double 

// Rational function (see More than a meal...)
double nti_ratfun(double *ab,  // abundance of nt interactor
                  double *ab0, // typical abundance of nt interactor
                  double *p,  // parameter, trophic     
                  double *dp) // parameter, non-trophic 
{ 
  
  return ( (*p + *dp) * *ab + *p * *ab0 ) / 
             ( *ab + *ab0 ); 
  
}
