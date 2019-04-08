#include <Rcpp.h>
using namespace Rcpp;

// Sets values of a matrix below eps to zero
// [[Rcpp::export]]
NumericMatrix zero_below_cpp(NumericMatrix old, 
                             double eps) {
  
  for (int i=0; i<old.nrow(); i++) { 
    for (int j=0; j<old.ncol(); j++) { 
      if (old(i,j) < eps) { 
        old(i,j) = 0.0;
      }
    }
  }
  
  // return the new matrix
  return old;
}
