# 
# Takes a system and a state, and runs it till equilibrium is reached.
# 

# Run a system
run <- function(syslist, times) {
  
  init_ode <- last_state(syslist)
  
  new.values <- with(syslist,
                     ode(y=init_ode, 
                         times=seq(time, ceiling(time_to_eq), by=tres),
                         func=func,
                         parms=NULL))
  
  syslist[['state']] <- rbind(syslist[['state']], new.values)
  syslist[['time']]  <- ceiling(syslist[['time_to_eq']])
  
  return(syslist)
}

run_to_eq <- function(syslist, 
                      full.calc=TRUE, # compute every step, not just end state
                      tmax=Inf,       # Max time before bailing if eq not reached
                      parms=NULL) { 
# Note that this function runs the calculations twice, hence it is surely not 
# very efficient... <!todo!> !
  
  # Extract last state reached in deSolve format
  init_ode <- last_state(syslist)
  
  # Compute steady state
  solution <- with(syslist,
                   runsteady(init_ode, 
                             func=func, 
                             times=c(time,tmax), 
                             parms=NULL))
  
  # Save info on system
  syslist[['time_to_eq']] <- attr(solution, 'time')
  
  # Get the state
  if (!full.calc) {
    syslist[['state']] <- solution$y
  } else { 
    times <- run(syslist, seq(time, ceiling(time_to_eq), by=tres))
    syslist <- run(syslist, times)
  }
  
  return(syslist)
}
