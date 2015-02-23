# 

# Get the last state of a system as a matrix to feed to solvers
last_state <- function(syslist,with.time=FALSE) {
  if (with.time) { 
    with(syslist, state[nrow(state),  , drop=FALSE])
  } else { 
    with(syslist, state[nrow(state),-1, drop=FALSE])
  }
}
  
# Get the last state of a system as a df to feed to ddply
last_state_df <- function(syslist,with.time=FALSE) {
  if (with.time) { 
    with(syslist, state[nrow(state), ])
  } else { 
    with(syslist, state[nrow(state),-1])
  }
}

# Get the last time value of a system
last_time <- function(syslist) {
  stop('Nope. Do not use last_time.') # <!todo!>
  with(syslist, state[nrow(state),1])
}

# Export data to a more practical data.frame 
export <- function(result,
                   name.skel="sp", 
                   gather=TRUE,
                   gather.key=name.skel,
                   gather.value="ab") { 
  
  if ( ! inherits(result,'deSolve')) 
    stop('Pass a deSolve object bro')
  
  # Format as df and set correct names
  df <- as.data.frame(result)
  node.names <- paste0(name.skel,seq.int(ncol(result)-1))
  names(df) <- c("time", node.names)
  
  # Gather if asked for it 
  df <- tidyr::gather_(df, gather.key, gather.value, node.names)
  
  return(df)
}
