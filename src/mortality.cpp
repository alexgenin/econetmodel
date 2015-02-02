
// Defines a logistic growth for a species
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mortality(NumericVector X, NumericVector m) { 
  
  int N = X.size(); 
  NumericVector dX(N); // variation of X as result of logi growth
  
  // Compute values of the output matrix
  for (int i=0; i<N; i++) {
    dX(i) = - X(i) * m(i);
  }
  
  return dX;
}
