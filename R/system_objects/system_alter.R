
# 
alter_system <- function(syslist, ...) { 
  alter_list(syslist, ...)
}

# Alter the parameters of a system
alter_parms_ <- function(syslist, new_parms) { 
  
  # Replace parms with new ones 
  syslist[['parms']] <- alter_list_(syslist[["parms"]], new_parms)
  
  # Recompile parameters if the system is compiled 
  if (inherits(syslist, 'system.compiled')) {
    syslist[['solver_parms']][['parms']] <- prepare_parameters(syslist[['parms']])
  }
  
  return(syslist)
}
alter_parms <- make_dotted(alter_parms_)

# Alter al list: standard evaluation equivalent 
alter_list_ <- function(list, new_elems) { 
  
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
# Same version but new_elems is passed as dots
alter_list <- make_dotted(alter_list_)
