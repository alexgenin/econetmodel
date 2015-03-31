
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
