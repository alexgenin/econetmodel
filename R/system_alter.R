# 
# Some operations to include when running systems
# 

remove_species <- function(syslist, which) { 
  syslist[['state']][nrow(syslist[['state']]),which+1] <- 0
  return(syslist)
}

state_alter <- function(syslist,newstate) {
  syslist[['state']][nrow(syslist[['state']]), ] <- c(syslist[['time']], newstate)
  return(syslist)
}

reset_time <- function(syslist,newtime=0) {
  syslist[['state']] <- last_state(syslist, with.time=TRUE)
  syslist[['time' ]] <- newtime
  return(syslist)
}

alter <- function(list, add=FALSE, ...) { 
  modifs <- match.call(expand.dots=FALSE)[['...']]
  list[names(modifs)] <- lapply(modifs,eval)
  return(list)
}
