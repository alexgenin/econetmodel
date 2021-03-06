# 
# Takes a system and a state, and runs it till equilibrium is reached.
# 

# Run a system multiple times
mrun <- function(sys, N, 
                 runfun=run, 
                 simplify=TRUE,
                 .parallel=FALSE, # catch the parallel arg
                 ...) { 
  
  # If .parallel=TRUE, then we load the library in R workers
  if (.parallel && exists(".LOCALCLUST")) {
    clusterEvalQ(.LOCALCLUST, dyn.load(sys[["library"]]))
  } else { 
    .parallel <- FALSE;
  }
  # Make a list of runs
  runids <- as.list(seq.int(N))
  
  # Do the runs and add their id in the resulting tab
  results <- llply(runids, 
                   function(id, runfun, sys, ...) { 
                     runfun(sys, ...) %>% add_id(id)
                   }, runfun, sys, 
                   .parallel = .parallel, ...)
  
  if (simplify) { 
    results <- do.call(rbind, results)
  }
  
  results
}

add_id <- function(result, id, sys = attr(result, 'system')) { 
  result_new <- cbind(id, result)
  with_system_attr(result_new, sys)
}

# Run a system for a given amount of time or along a vector of time
run <- function(sys, ...) {
  
  # Handle time specification
  times <- seq(get_tmin(sys), get_tmax(sys), by = get_timestep(sys))
  
  result <- do.call(ode,
                    c(list(y = get_state(sys)), 
                      list(times = times),
                      list(parms = vectorize_parameters(sys[['parms']], get_size(sys)) ),
                      get_solver_parms(sys),
                      ...))
  
  attr(result, "system") <- sys
  
  with_system_attr(result, sys)
}

