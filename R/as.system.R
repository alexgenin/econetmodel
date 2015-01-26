
# Creates a system function suitable for ode() given a list of nodes
as.system <- function(sysfun, floor=0) { 
  wrap_do.call <- function(f,...) { do.call(f,list(...)) } # helper function for mapply
  
  # Apply the limit function provided. The default is to limit species 
  # Return the system function to be passed to ode
  function(time, state, params) {
    state[state<floor] <- 0
    out <- sysfun(state)
    return(list(out))
  }
}
