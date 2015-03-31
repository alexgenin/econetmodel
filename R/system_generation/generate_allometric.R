# 
# This file contains stuff to generate systems based on allometry.
# 

# We use flo's manuscript as reference + Sonia's trophic code for atks.
gen_allometric_system <- function(bodyms,
                                  topology,
                                  mr0=0.314,
                                  mr0exp=-.25,
                                  a0=27.23734,
                                  a0exp=.25,
                                  eps=0.01064,
                                  h0=0.4,
                                  h0exp=-.75) { 
  
  if ( unique(c(length(bodyms), 
                nrow(topology),  # any different values
                ncol(topology))) != length(bodyms) ) { 
    stop("Parameters sizes mismatch.")
  }
  
  # Generate metabolic rates 
  xs <- mr0 * bodyms^mr0exp
  
  # Generate attack rates (based on Sonia's code)
  atk.rates <- topology
  Nsp <- length(bodyms)
  for (i in seq.int(Nsp)) { 
    for (j in seq.int(Nsp)) { 
      atk.rates[i,j] <- topology[i,j] * a0 * xs[i] * 
                          exp( - eps * bodyms[j] / bodyms[i] ) 
    }
  }
  
  # Generate handling times
  h <- h0*bodyms^h0exp
  
  # Return list
  list(x=xs, atk=atk.rates, h=h)
  
}
