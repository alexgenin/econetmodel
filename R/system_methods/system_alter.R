
# Add the option to remove a species to a system
set_removal <- function(sys, species, at=3000) { 
  
  # Define removal event for solver
  event <- list(func = 'remove_species',
                root = FALSE,
                time = nearestEvent(at, 
                                    seq(0, get_tmax(sys),
                                        by=get_timestep(sys)))) # the root max nb of eq 
  
    sys %>% 
      alter_system(solver_parms=list(events=event)) %>% 
      alter_parms(removed_species=species,
                  removed_species_total=length(species)) 
}

# Alter some components of a system
alter_system <- function(syslist, ...) { 
  alter_list(syslist, ...)
}

# Alter the parameters of a system and takes care of keeping the copy used by 
# the solver in sync.
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
    # If both args for this name are lists, we recurse.
    if ( is.list(list[[name]]) && is.list(new_elems[[name]]) ) { 
      list[[name]] <- alter_list_(list[[name]], new_elems[[name]])
    } else {
      list[[name]] <- new_elems[[name]]
    }
  }
  
  return(list)
}
# Same version but new_elems is passed as dots
alter_list <- make_dotted(alter_list_)
