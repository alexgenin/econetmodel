# 

# Generate stuff from body masses

# Metabolic rates
gen_met_rates <- function(bodyms, 
                          mr0=0.2227, 
                          mr0exp=-.25) { 
  mr0*bodyms^mr0exp
}


gen_atk_rates <- function(bodyms, 
                          xs,  # metab rates (see above)
                          a0=27.23734,
                          eps=0.01064) { 
  N <- length(xs);
  atks <- matrix(0,N,N)
  
  for (i in seq.int(N)) { 
    for (j in seq.int(N)) { 
      atks[i,j] <- a0 * xs[i] * exp(-eps * bodyms[i] / bodyms[j])
    }
  }
  atks
}
