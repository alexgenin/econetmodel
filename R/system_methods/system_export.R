# 
# Functions that take a system object and return understandable stuff
# 

biomass_by_tlvl <- function(result, trophlvls, fun=mean, ...,
                             sys=attr(result, "system")) { 
  
  # get species cols
  species_cols <- seq.int(get_size(sys))+1
  
  output <- matrix(NA_real_, nrow=nrow(result), ncol=length(unique(trophlvls))) 
  colnames(output) <- paste0("total_",unique(trophlvls))
  
  for (i in seq.int(length(unique(trophlvls)))) { 
    lvl <- unique(trophlvls)[i]
    output[ ,i] <- apply(result[ , species_cols[trophlvls %in% lvl]], 1, fun)
  }
  
  return( with_system_attr(cbind(result, output), sys) )
}

# Remove unstable simulations
discard_if_extinct <- function(result, before, 
                               extinct_threshold,
                               sys=attr(result,"system")) { 
                                 
  # Get simu ids with 
  has_extinctions <- any(result[last_before(before, result[ ,'time']), 
                                seq.int(get_size(sys))+1] < extinct_threshold)
    
  if (has_extinctions) {
  
#     print("discarded")
    
    if (is.matrix(result)) { 
      result[ , ] <- NA
    } else { 
      # We want to keep the factor columns intact
      result[ ,sapply(result, is.numeric)] <- NA
    }
    return( with_system_attr(result, sys) )
  } else { 
    return(result)
  }
  
}

# Insert the removal situation
insert_removal_case <- function(result, as,
                                sys=attr(result, "system")) { 
  
  rmsp <- which(get_parms(sys)[['removed_species']] > 0)
  
  # Create matrix with removal cases
  result_new <- cbind(as.data.frame(result), paste0(rmsp,collapse='') )
  colnames(result_new) <- c(colnames(result), as)
  
  with_system_attr(result_new, sys)
}

# Count the number of secondary extinctions
insert_sec_extinctions <- function(result, 
                                   removal_time=sys[['removal_time']],
                                   removal_species=get_parms(sys)[['removed_species']],
                                   sys=get_sys(result)) { 
  
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
  with_system_attr(result, sys)
}

insert_effect_size <- function(result, 
                               removal_time=sys[["removal_time"]], 
                               removal_species=get_parms(sys)[['removed_species']],
                               sys=get_sys(result)) { 
                                 
  # Warn if not enough info 
  if ( removal_time >= max(result[ ,'time']) ) {
    warning("Not enough information to compute effect size. Returning NA.")
    secextincts <- NA_real_
    
  } else { 
    
    # Find nearest line before removal time
    state_before <- result[last_before(removal_time, result[ ,'time']), ]
    state_after  <- result[nrow(result), ] # last state
    
    # Count secondary extinct species
    spcols <- seq.int(get_size(sys)) + 1 # first col is time, xx nexts are nodes
    effect_size <- sum( abs(state_after[spcols] - state_before[spcols]) )
  }
  
  # Add it to result matrix
  result <- cbind(result, effect_size=rep(effect_size, nrow(result)))
  
  with_system_attr(result, sys)
}



# Insert the parameters as columns in the output matrix
insert_parms <- function(result, ..., sys=get_sys(result)) { 
  
  # Stop if sys is not found
  if (is.null(sys)) stop("System object not found. Pass explicitely with sys=")
  
  # Get params from system
  to_insert <- as.list(match.call(expand.dots=FALSE))[["..."]] 
  to_insert <- lapply(to_insert, eval, 
                      envir=prepare_parms(get_parms(sys), get_size(sys)))
  
  # Select and bind subsets of parms. Note that unlist() conveniently add numbers
  # when elements of to_insert are vectors. 
  # The system attribute is transfered
  result <- do.call(cbind, c(list(result), unlist(to_insert)))
  
  # Transfer attr and return
  with_system_attr(result, sys)
}



# Select time range(s) (so unreadable)
select_ranges <- function(result, ..., 
                          sys=get_sys(result),
                          add.factor=FALSE, 
                          summarise.fun=identity) { 
  
  ranges <- match.call(expand.dots=FALSE)[['...']]
  ranges <- lapply(ranges, eval, envir=sys, enclos=parent.frame())
  
  # Create new tab 
  result.new <- lapply(ranges, 
                       function(r) { 
                         if (length(r) == 1) { 
                           result[ismin(abs(result[,"time"]-r)) , , drop=FALSE]
                         } else { 
                           result[result[ ,"time"] >= min(r) & 
                                  result[ ,"time"] <= max(r), ]
                       }})
  
  # Summarise if needed
  result.new <- lapply(result.new, aaply, 2, summarise.fun, .drop=FALSE)
  result.new <- lapply(result.new, t)
  
  if (add.factor == TRUE) { 
    
    # Add range names
    result.new <- mapply(function(e, name) { 
                           e <- as.data.frame(e)
                           e[ ,'range'] <- name
                           e 
                         }, 
                         result.new, names(ranges), 
                         SIMPLIFY=FALSE)
    
    # Bind into a single df and convert to factor 
    result.new <- do.call(rbind, result.new)
    result.new[ ,'range'] <- with(result.new, reorder(range, result.new[ ,1]))
  
  } else { 
    
    result.new <- do.call(rbind, result.new)
  }
  
  # Transfer attr and return
  with_system_attr(result.new, sys)
}

# Adjust the names of a deSolve result (mat or df) by adding a prefix to them
adjust_names <- function(result, prefix="sp", sys=attr(result,'system')) { 
  # Format column names
  cols <- colnames(result)
  nodecols <- seq.int(get_size(sys))+1
  cols[nodecols] <- paste0(prefix, cols[nodecols])
  # Insert
  colnames(result) <- cols
  
  with_system_attr(result,sys)
}

# Zero below the threshold
zero_below <- function(result, threshold, sys=get_sys(result)) { 
  
  spcols <- seq.int(get_size(sys)) + 1
  result[ ,spcols][result[ ,spcols] <= threshold] <- 0
  
  with_system_attr(result,sys)
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
with_system_attr <- function(result,sys) { 
  attr(result, "system") <- sys
  result
}
