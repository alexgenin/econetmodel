//
// 
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rockyshore(NumericVector times, // time vector
                NumericVector X,     // state vector
                List p) { // List of parameters
  
  int N = X.size(); 
  NumericVector dX(N); // variation of X 
  
  // Import parameters 
  // -----------------
  NumericVector rs  = as<NumericVector>(p["rs"]);
  NumericVector Ks  = as<NumericVector>(p["Ks"]);
  NumericMatrix ats = as<NumericMatrix>(p["ats"]);
  NumericMatrix ws  = as<NumericMatrix>(p["ws"]);
  NumericMatrix es  = as<NumericMatrix>(p["es"]);
  NumericVector hs  = as<NumericVector>(p["hs"]);
  NumericVector xs  = as<NumericVector>(p["xs"]);
  
  // Compute output values matrix 
  // -----------------------------------
  
  // Logistic growth of producers
  for (int i=0; i<N; i++) { // for each species
    dX(i) = X(i) * rs(i) * ( 1 - X(i) / Ks(i) );
  }
  
  // Species get eaten
  for (int i=0; i<N; i++) { // for each species...
    for (int j=0; j<N; j++) { // ...for each of its predators
      dX(i) -= ( ws(j,i) * ats(j,i) * X(i) * X(j) ) / 
                 ( es(j,i) * ( 1 + ats(j,i) * hs(j) * X(i) ) );
    }
  }
  
  // Species eat
  for (int i=0; i<N; i++) { // for each species...
    for (int k=0; k<N; k++) { // ...for each of its predators
      dX(i) += ( ws(i,k) * ats(i,k) * X(i) * X(k) ) / 
                 ( es(i,k) * ( 1 + ats(i,k) * hs(i) * X(k) ) );
    }
  }
  
  return List::create(dX);
}

