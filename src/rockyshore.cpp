//
// 
#include <Rcpp.h>
#include <limits>

static int dbg=0; 
// static double zerotol=std::numeric_limits<double>::epsilon();
static double zerotol=.001;

using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix fresp_type2(NumericVector X,
                          NumericMatrix atk,
                          NumericVector h) 
{
  int N=X.size();
  NumericMatrix fr(N,N);
  
  for (int i=0; i<N; i++) { 
    for (int j=0; j<N; j++) { 
      fr(i,j) = (atk(i,j) * X(j)) / 
                  (1 + atk(i,j) * h(j) * X(j));
    }
  }
  
  return fr;
}

// [[Rcpp::export]]
List rockyshore(NumericVector times, // time vector
                NumericVector X,     // state vector
                List p) { // List of parameters
  
  int N = X.size(); 
  
  NumericVector dX(N); // variation of X 
  
  // Import parameters 
  // -----------------
  NumericVector rs   = as<NumericVector>(p["rs"]);
  NumericVector Ks   = as<NumericVector>(p["Ks"]);
  NumericMatrix atks = as<NumericMatrix>(p["atks"]); // not "as" which is a reserved keyword
  NumericMatrix ws   = as<NumericMatrix>(p["ws"]);
  NumericMatrix es   = as<NumericMatrix>(p["es"]);
  NumericVector hs   = as<NumericVector>(p["hs"]);
  NumericVector xs   = as<NumericVector>(p["xs"]);
  
  // Compute functional responses
  // -----------------------------------
  for (int i=0; i<N; i++) { 
    if (X(i) <= zerotol) {
      X(i) = 0;
    }
  }
  
  // Compute functional responses
  // -----------------------------------
  NumericMatrix fr(N,N);
  fr = fresp_type2(X, atks, hs);
  
  // Compute output values matrix 
  // -----------------------------------
  
  // Logistic growth of producers
  for (int i=0; i<N; i++) { // for each species
    dX(i) = X(i) * rs(i) * ( 1 - X(i) / Ks(i) );
  }
  
  // Species get eaten
  for (int i=0; i<N; i++) { // for each species...
    for (int j=0; j<N; j++) { // ...for each of its predators
      dX(i) += - (X(j) * ws(j,i) * fr(j,i)) / 
                   es(j,i);
    }
  }
  
  // Species eat
  for (int i=0; i<N; i++) { // for each species...
    for (int j=0; j<N; j++) { // ...for each of its preys
      dX(i) += (X(i) * ws(i,j) * fr(i,j)) / 
                   es(i,j);
    }
  }
  
  // Species die because of metabolism
  for (int i=0; i<N; i++) { // for each species...
    dX(i) += - xs(i) * X(i);
  }
  // NB: this assumes plant die too 
  
  // Debug
  if (dbg) { 
    for (int i=0; i<N; i++) { 
      Rprintf("%f | %f ", X(i), dX(i));
    }
    Rprintf("\n");
  }
  
  return List::create(dX);
}
