
# 
alter_system <- function(syslist, new_components, 
                         recompile="auto") { 
  
  syslist <- alter_list(syslist, new_components)
  
  if (recompile == "auto" || recompile) { 
    
    do <- any(c('tags_replace','parms') %in% names(new_components))
    if (do) syslist <- compile.system(syslist)
    
  }
  
  return(syslist)
}

# Alter the parameters of a system
alter_parms <- function(syslist, parms.modifs) { 
  
  # Replace parms with new ones 
  new_parms <- alter_list(get_parms(syslist), parms.modifs) 
  
  syslist[['parms']] <- new_parms
  
  # Recompile parameters if the system is compiled 
  if (inherits(syslist, 'system.compiled')) {
    syslist[['solver_parms']][['parms']] <- prepare_parameters(new_parms)
  }
  
  return(syslist)
}

# Alter a deep list (depth_list>1)
alter_list <- function(list, new_elems) { 
  
  for (name in names(new_elems)) { 
  
    # If there is a sublist
    if (is.list(list[[name]])) { 
      list[[name]] <- alter_list(list[[name]], new_elems[[name]])
    # base case
    } else {
      list[[name]] <- new_elems[[name]]
    }
    
  }
  
  return(list)
}


# Alter a flat list
alter_flat_list <- function(list, new_elems) { 
  for (name in names(list)) { 
    list[[name]] <- new_elems[[name]]
  }
  
  return(list)
}

# Get the maximum depth of a list
depth <- function(l) { 
  if (any(unlist(lapply(l, is.list)))) { 
    depths_of_sublists <- lapply(l, depth_list)
    depth <- 1 + Reduce(max, depths_of_sublists)
  } else { 
    depth <- 1
  }
  return(depth)
}
