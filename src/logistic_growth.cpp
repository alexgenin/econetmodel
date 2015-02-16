/*
// Defines a logistic growth for a species
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector loggrowth(NumericVector X, NumericVector R, NumericVector K) { 
  
  int N = X.size(); 
  NumericVector dX(N); // variation of X as result of logi growth
  
  // Compute values of the output matrix
  for (int i=0; i<N; i++) {
    dX(i) = X(i) * R(i) * ( 1 - X(i)/K(i) );
  }
  
  return dX;
}*/
