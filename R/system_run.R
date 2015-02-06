# 
# Takes a system and a state, and runs it till equilibrium is reached.
# 

# Run a system for a given amount of time or along a vector of time
run <- function(syslist, times, method=rkMethod('ode45')) {
  
  init_ode <- last_state(syslist)
  
  # Handle time specification
  if (length(times) == 1) { 
    times <- with(syslist, seq(time, time+times, by=tres))
  } 
  
  new.values <- with(syslist,
                     ode(y=init_ode, 
                         times=times,
                         func=func,
                         parms=parms,
                         method=method))
  
  syslist[['state']] <- rbind(syslist[['state']], new.values)
  syslist[['time']]  <- new.values[nrow(new.values), 1]
  
  return(syslist)
}

# Run a system to equilibrium
run_to_eq <- function(syslist, 
                      full.calc=TRUE, # retrieve every step, not just end state
                      tmax=Inf) { 
# Note that this function runs the calculations twice, hence it is surely not 
# very efficient... <!todo!> !
  
  # Extract last state reached in deSolve format
  init_ode  <- last_state(syslist)
  
  # Compute steady state
  solution <- with(syslist,
                   runsteady(init_ode, 
                             func=func, 
                             times=c(time,tmax), 
                             parms=parms))
  
  # Save info on time at eq
  time_to_eq <- attr(solution, 'time')
  
  # Get the final state or all the steps
  if (!full.calc) {
    new.solution <- matrix(c(time_to_eq, solution$y),
                           dimnames=list(NULL, colnames(syslist[['state']])),
                           nrow=1)
    syslist[['state']] <- new.solution
    syslist[['time']]  <- time_to_eq
    return(syslist)
    
  } else { 
    times <- with(syslist, seq(time, time_to_eq, by=tres))
    return(run(syslist, times))
    
  }
  
}
