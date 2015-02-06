
# Create a state-representing object 
create_system <- function(sysfun,
                          init_time,
                          init_state,
                          parms=NULL,
                          ...,
                          tres=1, # time resolution
                          node_names_base='node') {
  
  N <- length(init_state)
  
  if (is.null(names(init_state))) { 
    names(init_state) <- paste0(node_names_base, seq.int(N))
  } 
  
  # Create state matrix and adjust names
  state <- matrix(c(time=init_time, init_state), ncol=N+1)
  colnames(state) <- c('time', names(init_state))
  
  syslist <- list(time=init_time, 
                  state=state, 
                  func=sysfun, 
                  tres=tres, 
                  parms=parms, 
                  ...)
  return(syslist)
}

syslist_create <- function(sysfun, init.time, X, parms=NULL, 
                           ..., 
                           tres=1, 
                           state.names.skl="node") { 
  warning('syslist_create is deprecated <!todo!>') #<!todo!>
  
  create_system(sysfun, 
                init_time=init.time, 
                init_state=X,
                parms=parms,
                tres=tres,
                node_names_base=state.names.skl)
}
                           