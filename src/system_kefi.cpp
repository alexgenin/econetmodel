//
// 
#include <Rcpp.h>
using namespace Rcpp;

//
// Do not forget to update the R wrapping function !
// 

// [[Rcpp::export]]
List system_kefi(NumericVector times, // time vector
                 NumericVector X,     // state vector
                 List p // List of parameters
                 ) { 
  
  int N = X.size(); 
  NumericVector dX(N); // variation of X to 
  
  // Import parameters
  NumericVector rs = as<NumericVector>(p["rs"]);
  NumericVector Ks = as<NumericVector>(p["Ks"]);
  
  // Compute values of the output matrix
  
  // Logistic growth of producers
  for (int i=0; i<N; i++) { // for each species
    dX(i) = X(i) * rs(i) * ( 1 - X(i) / Ks(i) );
  }
  
  
  return List::create(dX);
}
