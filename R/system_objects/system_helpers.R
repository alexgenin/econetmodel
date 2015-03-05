
# Export data to a more practical data.frame 
export <- function(result,
                   name.skel="sp", 
                   gather.do=TRUE,
                   gather.key=name.skel,
                   gather.value="ab") { 
  
  if ( ! inherits(result,'matrix')) 
    stop('Pass a matrix bro')
  
#   # Zero really small stuff
#   result[abs(result) < 10e-10 | result <= 0] <- 0;
  
  df <- as.data.frame(result)
  
  # Adjust names
  nodecols <- is_nodecol(colnames(result))
  names(df) <- adjust_names(colnames(result), name.skel)
  
  # Gather if asked for it 
  if (gather.do) { 
    df <- tidyr::gather_(df, gather.key, gather.value, names(df)[nodecols])
  }
  
  return(df)
}

# Adds a name to the single digit columns of a deSolve result
adjust_names <- function(result.names, 
                         name.skel="sp") { 
  nodecols <- is_nodecol(result.names)
  result.names[nodecols] <- paste0(name.skel, result.names[nodecols])
  
  return(result.names)
}

# Currently tests if single digit
is_nodecol <- function(current.names) { 
  return( grepl('^[0-9]{1}$', current.names) )
}
