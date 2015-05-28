# 
# Functions to help analysings secondary extinctions
# 

count_sec_extinctions <- function(result, 
                                  
                                  species_cols, 
                                  removal_cols) { 
  
  # Get column names
  removal_cols <- dplyr::select_vars_(colnames(result), substitute(removal_cols))
  species_cols <- dplyr::select_vars_(colnames(result), substitute(species_cols))
  
  # A species is secondary extinct if its abundance is 0 but it has not been 
  # removed artificially.
  is_extinct  <- result[ ,species_cols, drop=FALSE] == 0
  was_removed <- result[ ,removal_cols, drop=FALSE] > 0
  
  # We return a matrix that indicates whether the species is extinct or not
  return( is_extinct & (! was_removed) )
}

removal_cases <- function(result, removal_cols) { 
  case <- ''
  for (i in seq.int(8)) { 
    case <- paste0(case, ifelse(dat[, paste0('removedspecies',i)] > 0, i, ''))
  }
  dat[ ,'removal'] <- case
  dat
}
