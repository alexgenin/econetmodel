# 
# This file contains stuff to generate systems based on allometry.
# 

gen_allometric_system <- function(bodyms,
                                  topology,
                                  mr0=0.2227,
                                  mr0exp=-.25,
                                  a0=27.23734,
                                  eps=0.01064,
                                  y=8) { 
  
  if ( unique(c(length(bodyms), 
                nrow(topology),  # any different values
                ncol(topology))) != length(bodyms) ) { 
    stop("Parameters sizes mismatch.")
  }
  
  # Generate metabolic rates 
  xs <- gen_met_rates(bodyms, mr0, mr0exp)
  
  # Generate attack rates
  atk.rates <- topology * gen_atk_rates(bodyms, xs, a0, eps)
  
  # Generate handling times
  h = 1/(y*xs)
  
  # Return list
  list(x=xs, atk=atk.rates, h=h)
  
}


# Metabolic rates
gen_met_rates <- function(bodyms, 
                          mr0=0.2227, 
                          mr0exp=-.25) { 
  mr0 * bodyms^mr0exp
}

# Attack rates
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
