# 
# 
# This file contains functions that generate interaction matrices
# 

# Generate a random matrix
interact_random <- function(sys, rfun, ...) { 
  size <- get_size(sys)
  matrix(rfun(size^2, ...), size, size)
}

# Generate interaction coefficients that are below a certain value
# The result of all interactions must be always below the metabolic rate x
interact_below <- function(sys, mins=-Inf, maxs=Inf, rfun, ...) { 
  
  maxs <- eval(substitute(maxs), envir=get_parms(sys), enclos=parent.frame())
  mins <- eval(substitute(mins), envir=get_parms(sys), enclos=parent.frame())
  size <- get_size(sys)
  
  browser()
  if ( length(mins) == 1 ) mins <- rep(mins, size)
  if ( length(maxs) == 1 ) maxs <- rep(maxs, size)
  
  output <- matrix(0, size, size)
  for (i in seq.int(size)) { 
    output[i, ] <- within(mins[i], maxs[i], rfun)(size, ...)
  }
  
}
# interact_below(base_system, 0, x, runif, 0, 10)

# Get a vector of random values within a specified range
within <- function(min, max, rfun) {
  function(n, ...) { 
    if (min == max) return(rep(min, n))
  
    bad_values <- rep(TRUE, n) # replace all values at init
    values <- rep(0,n)
    while ( any(bad_values) ) { 
      cat(sum(bad_values),"\n")
      values[bad_values] <- rfun(sum(bad_values), ...) 
      bad_values <- (values < min) | (values > max)
    } 
    return(values)
  }
}