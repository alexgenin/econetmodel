
#include <R.h>

// This file contains functional responses
// #include "./include/functional_response.h"


// Given two species, get the functional response value of the interaction i->j
// This is a type 2 functional response of the form  : 
// F = aR / (1+ahR)
double frtype2(double *prey,    // abundance of ressource (pointer)
               double *atk,     // attack rate            (pointer)
               double *h) {     // handling time          (pointer)
  
  double fr = 0;
  
  fr = (*prey * *atk) / (1 + (*h * *prey * *atk));
  
//   Rprintf("%f | %f \n", *prey, fr); // dbg
  
  return fr;
}
