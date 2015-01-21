
# Creates a system function suitable for ode() given a list of nodes
as.system <- function(nodelist) { 
  ids <- as.list(seq.int(nodelist))
  wrap_do.call <- function(f,...) { do.call(f,list(...)) }
  
  function(time, state, params) {
    out <- mapply(wrap_do.call, nodelist, ids, MoreArgs=list(state))
    return(list(out))
  }
}
