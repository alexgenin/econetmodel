// 
// Generate parameters from a body mass
// 
#include <Rcpp.h>
#include <math.h> // pow, etc.

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bmgen_mrates(NumericVector bodyms, // body masses
           double mr0,     // basal metab rate
           double mr0exp)  // exponent
{ 
  
  int N = bodyms.length();
  
  // Compute metabolic rates x
  NumericVector mr(N);
  for (int i=0;i<N;i++) { 
    mr(i) = mr0 * pow(bodyms(i),mr0exp);
  }
  
  return mr;
}

// [[Rcpp::export]]
NumericMatrix bmgen_arates(NumericVector bodyms, // body masses
                           NumericVector mrates, // metabolic rates
                           double a0,            // basal atk rate
                           double eps)           // exponent
{ 
  int N = bodyms.length();
  
  // Compute attack rates (matrix)
  NumericMatrix ats(N,N);
  for (int i=0;i<N;i++) { 
    for (int j=0;j<N;j++) { 
      ats(i,j) = a0 * mrates(i) * exp( -eps * bodyms(i) / bodyms(j));
    } 
  }
  
  return ats;
}
