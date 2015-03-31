
#include <R.h>
#include <exports.h> //pow

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
<<<<<<< HEAD
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
double frhill(double *prey, 
              double *sumpreyatk, 
              double *atk, 
              double *w,
=======
// Fr = a*prey^(1+q) / ( 1 + a*h*total^(1+q) )
double frhill(double *prey, 
              double *total_prey, 
              double *atk, 
              double *w, 
>>>>>>> master
              double *h,
              double *q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
<<<<<<< HEAD
   * sumpreyatk are non-negative, in which case pow returns NaN. It is up to 
=======
   * total_prey are non-negative, in which case pow returns NaN. It is up to 
>>>>>>> master
   * the calling function to check for this. 
   */
  
  double fr = 0;
<<<<<<< HEAD
  fr = *w * *atk * pow(*prey, 1.0 + *q) / (1 + (*h * *w * *sumpreyatk));
=======
  fr = *atk * *w * pow(*prey, 1.0 + *q) / 
         ( 1 + (*h * *w * pow(*total_prey, 1.0 + *q)) );
>>>>>>> master
  
  return fr;
}
