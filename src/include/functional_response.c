
#include <R.h>
#include <exports.h> //pow

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
void frhill(int nsp,
            int extant_species[nsp],
            int extant_n,
            double F[nsp][nsp],
            double y[nsp],
            double w[nsp][nsp],
            double atk[nsp][nsp],
            double h[nsp],
            double q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
   * sumpreyatk are non-negative, in which case pow returns NaN. It is up to 
   * the calling function to check for this. 
   */
  
  // Compute the sum of available preys 
  // We only loop over extant species here or otherwise the sum can be nan as
  // there is no guarantee that y[prey] > 0. This way we avoid the nans to 
  // propagate to correct F[i][j] values.
  int prey = 0;
  
  for (int i=0; i<nsp; i++) { 
    
    double sumpreyatk = 0;
    for (int k=0; k<extant_n; k++) { 
      prey = extant_species[k];
      sumpreyatk += atk[i][prey] * pow(y[prey], 1.0 + q);
    }
    
    // Compute the functional response
    for (int j=0; j<nsp; j++) { 
      F[i][j] = w[i][j] * atk[i][j] * pow(y[j], 1.0 + q) / 
                  ( 1 + ( w[i][j] * h[i] * sumpreyatk ) );
    }
  }
  
}
