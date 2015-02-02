
// Define functional responses in C++
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix frtype2(NumericVector X, NumericMatrix A, NumericMatrix B0) { 
// Functional response of the form 
  int N = X.size(); 
  NumericMatrix dX(N,N); // flow of biomass from j to i
  double den; // sum in the denominator of FR
  
  // Compute values of the output matrix
  for (int i=0; i<N; i++) {
    for (int j=0; j<N; j++) {
      // Computes the effect of j on i
      den = 0;
      for (int k=0; k<N; k++) {
        den += A(i,k) * X(k);
      }
    dX(i,j) = X(j) / ( den + B0(j,i) ); // B0(j,i) ?
    }
  }
  
  return dX;
}

