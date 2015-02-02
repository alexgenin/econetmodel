# 

# Get the last state of a system as a vector to feed to solvers
last_state <- function(syslist) {
  with(syslist, state[nrow(state),-1])
}
