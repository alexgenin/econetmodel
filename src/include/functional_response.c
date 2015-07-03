
#include <R.h>
#include <math.h> //pow

// We use a tolerance here as will be a double
#define is_non_zero(a) ( (a) <= 1e-5 ? 0.0 : 1.0 )

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
void frhill(int nsp,
            int extant_species[nsp],
            int extant_n,
            double F[nsp][nsp],
            double y[nsp],
            double atk[nsp][nsp],
            double h[nsp],
            double q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
   * sumpreyatk are non-negative, in which case pow returns NaN. It is up to 
   * the calling function to check for this. 
   */
  
  /* Compute the sum of available preys 
   * We only loop over extant species here or otherwise the sum can be nan as
   * there is no guarantee that y[prey] > 0. This way we avoid the nans to 
   * propagate to correct F[i][j] values.
   */
  
  for (int sp1=0; sp1<extant_n; sp1++) { 
    int i = extant_species[sp1];
    
    
    // Compute needed values for the functional response
    double sumprey_i = 0; 
    double sumpreyatk_i = 0;
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      // Total biomass of preys for i 
      sumprey_i += is_non_zero(atk[i][j]) * y[j];
      // Total biomass * atk
      sumpreyatk_i += atk[i][j] * pow(y[j], 1.0 + q);
//       Rprintf("i: %i j: %i atkij: %e yj: %e -> %e \n", 
//               i,    j,    atk[i][j],    y[j], sumprey_i);
    }
    
    // What if sumprey_i == 0 ?
    
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      if ( atk[i][j] >= 1e-5 ) { 
        F[i][j] = y[j] / sumprey_i * atk[i][j] * pow(y[j], 1.0 + q) / 
                    ( 1 + ( y[j] / sumprey_i * h[i] * sumpreyatk_i) );
      } else { 
        F[i][j] = 0.0;
      }
//       Rprintf("F[%i][%i]: %e\n", i, j, F[i][j]);
    }
    
  }
  
}
