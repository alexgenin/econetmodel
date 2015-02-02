# 

# Get the last state of a system as a vector to feed to solvers
last_state <- function(syslist) {
  with(syslist, state[nrow(state),-1])
}

# Get the last time value of a system
last_time <- function(syslist) {
  with(syslist, state[nrow(state),1])
}
