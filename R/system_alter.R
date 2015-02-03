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

reset_time <- function(syslist,newtime=0) {
  syslist[['state']] <- last_state(syslist, with.time=TRUE)
  syslist[['time' ]] <- newtime
  return(syslist)
}
