# 
# Functions that take a system object and return understandable stuff
# 

# Remove unstable simulations
discard_if_extinct <- function(result, before, 
                               extinct_threshold,
                               sys=attr(result,"system")) { 
                                 
  # Get simu ids with 
  has_extinctions <- any(result[last_before(before, result[ ,'time']), 
                                seq.int(get_size(sys))+1] < extinct_threshold)
  
  if (has_extinctions) {
    result <- result[nrow(result), , drop = FALSE]
    
    if ( is.matrix(result) ) { 
      result[] <- NA_real_
    } else { 
      # We want to keep the time column and factorial columns intact
      result[ ,sapply(result, is.numeric)] <- NA_real_
    }
  }
  
  return( with_system_attr(result, sys) )  
}

insert_removal_case_rs <- function(result, 
                                   prefix = 'rm', 
                                   sys = attr(result, "system")) { 
  force(sys)
  rmsp <- sort(which(get_parms(sys)[['removed_species']] > 0))
  species <- get_species(sys)
  result_new <- as.data.frame(result)
  
  # Insert into result
  removal_cols <- data.frame(rm1 = factor(rep("-",nrow(result_new)),
                                          levels = c('-',species[c(7,8)])),
                             rm2 = factor(rep("-", nrow(result_new)),
                                          levels = c('-',species[c(5,6)])))
  
  if ( any(c(7,8) %in% rmsp) ) { 
    removal_cols[1] <- species[max(rmsp)] # always 7 OR 8
  }
  
  if ( any(c(5,6) %in% rmsp) ) { 
    removal_cols[2] <- species[min(rmsp)] # always 5 or 6
  }
  
  result_new <- data.frame(result_new, removal_cols)
  attr(result_new, 'system') <- sys
  
  return(result_new)
}

# Count the number of secondary extinctions
insert_sec_extinctions <- function(result, 
                                   sys = get_sys(result),
                                   removal_time = sys[['removal_time']],
                                   removal_species=get_parms(sys)[['removed_species']]) { 
  
  # Warn if not enough info 
  if ( removal_time >= max(result[ ,'time']) ) {
    warning("Not enough information to compute the number of secondary",
            "extinctions. Returning NA.")
    secextincts <- NA_integer_
    
  } else { 
    
    # Find nearest line before removal time
    state_before <- result[last_before(removal_time, result[ ,'time']), ]
    state_after  <- result[nrow(result), ]
    
    # Count secondary extinct species
    spcols <- seq.int(get_size(sys)) + 1 # first col is time, xx nexts are nodes
    extincts_before <- state_before[spcols] == 0
    extincts_after  <- state_after[spcols] == 0
    species_removed <- removal_species > 0
    
    secextincts <- sum(extincts_after & ! extincts_before & ! species_removed)
  }
  
  # Add it to result matrix
  result <- cbind(result, secext=rep(secextincts, nrow(result)))
  with_system_attr(result, sys) # TODO: transfer attributes
}

# Insert the parameters as columns in the output matrix
insert_parms <- function(result, ..., sys = get_sys(result)) { 
  
  # Stop if sys is not found
  if (is.null(sys)) stop("System object not found. Pass explicitely with sys=")
  
  # Get params from system
  to_insert <- as.list(match.call(expand.dots = FALSE))[["..."]] 
  to_insert <- lapply(to_insert, eval, 
                      envir = prepare_parms(get_parms(sys), get_size(sys)))
  
  # Select and bind subsets of parms. Note that unlist() conveniently add numbers
  # when elements of to_insert are vectors. 
  # The system attribute is transfered
  result <- do.call(cbind, c(list(result), unlist(to_insert)))
  
  # Transfer attr and return
  with_system_attr(result, sys)
}


# Select time range(s) (so unreadable)
select_ranges <- function(result, ..., 
                          sys = get_sys(result),
                          add.factor = FALSE, 
                          summarise.fun = NULL) { 
  
  ranges <- match.call(expand.dots = FALSE)[['...']]
  ranges <- lapply(ranges, eval, envir=sys, enclos=parent.frame())
  
  if ( any(is.na(result[ ,2])) ) { # unstable result
    
    result.new <- matrix(rep(result, length(ranges)), 
                         nrow=length(ranges), byrow = TRUE)
    colnames(result.new) <- colnames(result)
    
    if (add.factor) { 
      result.new <- data.frame(range = names(ranges), result.new)
    }
    
  } else { 
    # Create new tab 
    result.new <- lapply(ranges, 
                        function(r) { 
                          if (length(r) == 1) { 
                            result[ismin(abs(result[ ,"time"]-r)), , drop=FALSE]
                          } else {
                            result[result[ ,"time"] >= min(r) & 
                                   result[ ,"time"] <= max(r),  ]
                        }})
    
    if ( ! is.null(summarise.fun) ) { 
      result.new <- lapply(result.new, aaply, 2, summarise.fun, .drop=FALSE)
      result.new <- lapply(result.new, t)
    }
    
    if (add.factor) { 
      
      for (range in names(result.new)) { 
        # The name of the new column is defined from the name of the variable, 
        # here "range".
        result.new[[range]] <- data.frame(range=range, result.new[[range]])
      }
      
    }
    
    result.new <- do.call(rbind, result.new)
  }
  
  if (add.factor) { 
    result.new[ ,'range'] <- reorder(result.new[ ,'range'], 
                                     result.new[ ,'time'])
  }
  
  rownames(result.new) <- as.character(seq_along(rownames(result.new)))
  
  # Transfer attr and return
  with_system_attr(result.new, sys)
}

# Adjust the names of a deSolve result (mat or df) by adding a prefix to them
adjust_names <- function(result, 
                         spnames = get_species(sys),
                         sys     = attr(result,'system')) { 
  
  # Format column names
  col_names <- colnames(result)
  nodecols  <- seq.int(get_size(sys)) + 1 # node columns
  col_names[nodecols] <- spnames
  
  colnames(result) <- col_names
  with_system_attr(result,sys)
}

# Zero below the threshold
zero_below <- function(result, threshold, sys = get_sys(result)) { 
  
  spcols <- seq.int(get_size(sys)) + 1
  result[ ,spcols][result[ ,spcols] <= threshold] <- 0
  
  with_system_attr(result, sys)
}


# Currently tests if single digit with possible prefix 
is_nodecol <- function(current.names, prefix="") { 
  grepl(paste0('^(',prefix,'|)[0-9]{1}$'), current.names)
}


# Get last time step before given time
last_before <- function(time,X) which(X >= time)[1] - 1

# Try to retrieve system from the system attribute of the result
get_sys <- function(result, attr.name='system') { 
  if (attr.name %in% names(attributes(result))) { 
    attr(result, attr.name) 
  } else { 
    stop("Could not find the system object from the passed result. Pass it",
         "explicitely through the sys= argument")
  }
}

# Transfer system attribute
with_system_attr <- function(obj, sys) { 
  attr(obj, "system") <- sys
  obj
}
