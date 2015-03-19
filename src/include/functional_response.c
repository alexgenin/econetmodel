
#include <R.h>
#include <exports.h> //pow

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
double frhill(double prey, 
              double total_prey, 
              double atk, 
              double h,
              double q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
  /* total_prey are non-negative, in which case pow returns NaN. It is up to 
   * the calling function to check for this. 
   */
  
  double fr = 0;
  fr = atk * pow(prey, 1.0 + q) / 
         (1 + (h * atk * pow(total_prey, 1.0 + q)));
  
  return fr;
}
