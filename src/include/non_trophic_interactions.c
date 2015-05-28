/* 
 * This file contains NTI-describing functions
 */
#include <string.h>
#include <R.h>

void nti_ratfun_vec(int nsp, 
                    int extant_species[nsp],
                    int extant_n,
                    double newp[nsp],
                    double ab[nsp],  // abundance of nt interactor
                    double ab0[nsp], // typical abundance of nt interactor
                    double p[nsp],  // parameter, trophic     
                    double dp[nsp][nsp]) { // parameter, non-trophic 

  
  // For all extant species i 
  for (int i=0; i < extant_n; i++) { 
    int sp = extant_species[i];
    
    // Default value is the one without NTI
    newp[sp] = p[sp];
    
    // For each extant species j having an effect on i
    for (int j=0; j < extant_n; j++) { 
      int inter = extant_species[j];
      
      // We get the value of the parameter with the trophic interaction. This is
      // equal to the original p when dp == 0 so we have to substract the 
      // original value (Trophic only) for p.
      double nt_modifier = 
        ( ( (p[sp] + dp[inter][sp]) * ab[inter] + p[sp] * ab0[inter] ) / 
          ( ab[inter] + ab0[inter] ) ) - p[sp];
      
      // We alter the value of the parameter 
      newp[sp] += nt_modifier;
    }
  
  }
}

// Same but p and newp are matrices
void nti_ratfun_mat(int nsp, 
                    int extant_species[nsp],
                    int extant_n,
                    double newp[nsp][nsp],
                    double ab[nsp],        // abundance of nt interactor
                    double ab0[nsp],       // typical abundance of nt interactor
                    double p[nsp][nsp],    // parameter, trophic only
                    double dp[nsp][nsp][nsp]) { // parameter, non-trophic 
  
  
  // For all extant species i 
  for (int i=0; i < extant_n; i++) { 
    int spi = extant_species[i];
    
    // For all extant species j
    for (int j=0; j < extant_n; j++) { 
      int spj = extant_species[j];
      
      // Default value is the one without NTI
      newp[spi][spj] = p[spi][spj];
      
      // For each extant species k having an effect on the interaction i/j
      for (int k=0; k < extant_n; k++) { 
        int inter = extant_species[k];
        
        // We get the value of the parameter with the trophic interaction. This is
        // equal to the original p when dp == 0 so we have to substract the 
        // original value (Trophic only) for p.
        double nt_modifier = 
          ( ( (p[spi][spj] + dp[inter][spi][spj]) * ab[inter] + p[spi][spj] * ab0[inter] ) / 
            ( ab[inter] + ab0[inter] ) ) - p[spi][spj];
        
        // We alter the value of the parameter 
        newp[spi][spj] += nt_modifier;
      }
      
    }
  }
}
