
#include <R.h>
#include <math.h> //pow

// We use a tolerance here as will be a double
#define one_if_non_zero(a) ( (a) <= 1e-5 ? 0.0 : 1.0 )
#define true_if_non_zero(a) ( (a) <= 1e-5 ? int 0 : int 1 )

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
void frhill(int nsp,
            int extant_species[nsp],
            int extant_n,
            double F[nsp][nsp],
            double y[nsp],
            double atk[nsp][nsp],
            double w[nsp][nsp],
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
//    double sumprey_i = 0; 
    double sumpreyatk_i = 0;
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      // Total biomass of preys for i 
      // sumprey_i += one_if_non_zero(atk[i][j]) * y[j];
      // Total biomass * atk
      sumpreyatk_i += atk[i][j] * pow(y[j], 1.0 + q);
//       Rprintf("i: %i j: %i atkij: %e yj: %e -> %e \n", 
//               i,    j,    atk[i][j],    y[j], sumprey_i);
    }
    
    // What if sumprey_i == 0 ?
    
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      if ( atk[i][j] >= 1e-5 ) { 
        F[i][j] = w[i][j] * atk[i][j] * pow(y[j], 1.0 + q) / 
                    ( 1 + ( w[i][j] * h[i] * sumpreyatk_i) );
      } else { 
        F[i][j] = 0.0;
      }
//       Rprintf("F[%i][%i]: %e\n", i, j, F[i][j]);
    }
    
  }
  
}


// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
// Fr = aR^(1+q) / ( 1 + ahR^(1+q) )
void frhill_dynw(int nsp,
            int extant_species[nsp],
            int extant_n,
            double F[nsp][nsp],
            double y[nsp],
            double atk[nsp][nsp],
            double w[nsp][nsp],
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
      sumprey_i += one_if_non_zero(atk[i][j]) * y[j] * w[i][j];
      // Total biomass * atk
      sumpreyatk_i += atk[i][j] * pow(y[j], 1.0 + q);
//       Rprintf("i: %i j: %i atkij: %e yj: %e -> %e \n", 
//               i,    j,    atk[i][j],    y[j], sumprey_i);
    }
    
    // What if sumprey_i == 0 ?
    
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      if ( atk[i][j] >= 1e-5 ) {
        
        double w_current = w[i][j] * y[j] / sumprey_i;
        
        F[i][j] = w_current * atk[i][j] * pow(y[j], 1.0 + q) / 
                    ( 1 + ( w_current * h[i] * sumpreyatk_i) );
      } else { 
        F[i][j] = 0.0;
      }
//       Rprintf("F[%i][%i]: %e\n", i, j, F[i][j]);
    }
    
  }
  
}



// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
void fryodzis(int nsp,
              int extant_species[nsp],
              int extant_n,
              double F[nsp][nsp],
              double y[nsp],
              double atk[nsp][nsp],
              double w[nsp][nsp],
              double h[nsp],
              double q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
   * sumpreyatk are non-negative, in which case pow returns NaN. It is up to 
   * the calling function to check for this. 
   */
  
  /* Compute the sum of available preys 
   * We only loop over extant species here or otherwise the sum can be nan as
   * there is no guarantee that y[prey] > 0. This way we avoid the nans 
   * propagating to correct F[i][j] values.
   */
    
  for (int sp1=0; sp1<extant_n; sp1++) { 
    int i = extant_species[sp1];
    
    // Compute needed values for the functional response
    double sumpreyatk_i = 0;
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int k = extant_species[sp2];
      sumpreyatk_i += w[i][k] * atk[i][k] * pow(y[k], 1.0 + q);
    }
    
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      
      if ( atk[i][j] >= 1e-5 ) {
        
        F[i][j] = w[i][j] * atk[i][j] * pow(y[j], 1.0 + q) / 
                    ( 1 + ( h[i] * sumpreyatk_i) );
      } else { 
        
        F[i][j] = 0.0;
      }
//       Rprintf("F[%i][%i]: %e\n", i, j, F[i][j]);
    }
    
  }
  
}

// Hill type functional response (allows for intermediate func responses between 
// 2 (q=0) and 3 (q=1)).
void fryodzis_dynw(int nsp,
                   int extant_species[nsp],
                   int extant_n,
                   double F[nsp][nsp],
                   double y[nsp],
                   double atk[nsp][nsp],
                   double w[nsp][nsp],
                   double h[nsp],
                   double q) { 
  
  /* Note: we use pow here, although there is no guarantee that prey or 
   * sumpreyatk are non-negative, in which case pow returns NaN. It is up to 
   * the calling function to check for this. 
   */
  
  /* Compute the sum of available preys 
   * We only loop over extant species here or otherwise the sum can be nan as
   * there is no guarantee that y[prey] > 0. This way we avoid the nans 
   * propagating to correct F[i][j] values.
   */

  // We compute the preferences of species i on j: this is variable depending
  // on the abundances of the js
  
  for (int sp1=0; sp1<extant_n; sp1++) { 
    int i = extant_species[sp1];
    
    double sumprey_i = 0; 
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      // Total biomass of preys for i * the preference
      sumprey_i += one_if_non_zero(atk[i][j]) * y[j] * w[i][j];
    }
    
    // Compute needed values for the functional response
    double sumpreyatk_i = 0;
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int k = extant_species[sp2];
      
      double wik = w[i][k] * y[k] / sumprey_i;
      
      sumpreyatk_i += wik * atk[i][k] * pow(y[k], 1.0 + q);
    }
    
    for (int sp2=0; sp2<extant_n; sp2++) { 
      int j = extant_species[sp2];
      
      if ( atk[i][j] >= 1e-5 ) {
        
        double wij = w[i][j] * y[j] / sumprey_i;
        
        F[i][j] = wij * atk[i][j] * pow(y[j], 1.0 + q) / 
                    ( 1 + ( h[i] * sumpreyatk_i) );
      } else { 
        
        F[i][j] = 0.0;
      }
//       Rprintf("F[%i][%i]: %e\n", i, j, F[i][j]);
    }
    
  }
  
}


