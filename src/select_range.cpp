#include <Rcpp.h>
using namespace Rcpp;

// Sets values of a matrix below eps to zero
// [[Rcpp::export]]
NumericMatrix select_range(NumericMatrix inmat, 
                           NumericVector range) {
  int nrows = inmat.nrow();
  
  // Determine subset to copy
  IntegerVector insubset(nrows);
  int output_size = 0;
  
  for (int i=0; i<nrows; i++) { 
    
    // First column of input matrix is always time !
    if ( (inmat(i,0) >= min(range)) && (inmat(i,0) <= max(range)) ) { 
      insubset(i) = 1;
      output_size++;
    } else { 
      insubset(i) = 0;
    }
  }
  
  // Copy data to the output subset
  NumericMatrix output(output_size, inmat.ncol()); 
  
  int to=0;
  for (int from=0; from<nrows; from++) { 
    if (insubset(from)) { 
      // If in subset, we copy values to the new matrix
      for (int j=0; j<inmat.ncol(); j++) { 
        output(to, j) = inmat(from, j);
      }
      to++;
    }
  }
  
  // Also transfer attributes (colnames in fact)
  // TODO: will crash if rownames are defined ? (because the 
  // size of the rownames vector will be different).
  output.attr("dimnames") = inmat.attr("dimnames");
  
  // return the new matrix
  return output;
}

