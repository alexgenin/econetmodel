
# Create a state-representing object 
syslist_create <- function(sysfun,init.time,X,
                           parms=NULL,
                           ...,
                           tres=1, # time resolution
                           state.names.skl='node') {
  
  N <- length(X)
  
  if (is.null(names(X))) { 
    names(X) <- paste0(state.names.skl, seq.int(N))
  } 
  
  # Create state matrix and adjust names
  state <- matrix(c(time=init.time, X), ncol=N+1)
  colnames(state) <- c('time', names(X))
  
  syslist <- list(time=init.time, 
                  state=state, 
                  func=sysfun, 
                  tres=tres, 
                  parms=parms, 
                  ...)
  return(syslist)
}

