# 
# Takes a system and a state, and runs it till equilibrium is reached.
# 

# Run a system for a given amount of time or along a vector of time
run <- function(sys, tmax, ...) {
  
  # Handle time specification
  times <- seq(get_time(sys), tmax, by=get_timestep(sys))
  
  new.values <- do.call(ode,c(list(y=get_state(sys)), 
                              list(times=times),
                              get_solver_parms(sys),
                              ...))
  
  return(new.values)
}
