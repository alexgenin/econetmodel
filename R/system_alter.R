# 
# Some operations to include when running systems
# 

remove_species <- function(syslist, which) { 
  syslist[['state']][nrow(syslist[['state']]),which+1] <- 0
  return(syslist)
}

state_alter <- function(syslist,newstate) {
  with(syslist, state[nrow(state), ] <- newstate)
  return(syslist)
}

