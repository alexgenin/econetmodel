# 
# Takes a system and a state, and runs it till equilibrium is reached.
# 



# Run a system multiple times
mrun <- function(sys, N, 
                 runfun=run, 
                 simplify=TRUE,
                 ...) { 
  
  # Make a list of runs
  runids <- as.list(seq.int(N))
  
  # Do the runs and add their id in the resulting tab
  results <- llply(runids, 
                    function(id, runfun, sys, ...) { 
                      runfun(sys, ...) %>% cbind(id,.) 
                    }, runfun, sys, ...)
  
  if (simplify) { 
    results <- do.call(rbind, results)
  }
  
  results
}

# Run a system for a given amount of time or along a vector of time
run <- function(sys, ...) {
  
  # Handle time specification
  times <- seq(get_time(sys), get_tmax(sys), by=get_timestep(sys))
  
  new.values <- do.call(ode,c(list(y=get_state(sys)), 
                              list(times=times),
                              get_solver_parms(sys),
                              ...))
  
  return(new.values)
}