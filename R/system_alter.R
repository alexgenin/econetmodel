
# Add layout to system
add_layout <- function(sys, x=NULL, y=NULL) { 
  
  # Choose a sensible default if not filled
  if (is.null(layout)) { 
    y <- rank(get_parms(sys)[["x"]], ties.method='min') - 1
    x <- y
  }
  sys[['layout']] <- data.frame(x=x,y=y)
  return(sys)
}

# Add the option to remove a species to a system
set_removal <- function(sys, species, at=3000) { 
  
  # Define removal event for solver
  event <- list(func = 'remove_species',
                root = FALSE,
                time = nearestEvent(at, seq(0, get_tmax(sys), 
                                    by=get_timestep(sys)))) 
  
  # Rebuild species removal vector
  to_remove <- rep(0, get_size(sys))
  to_remove[species] <- 1 
  
  sys %>% 
    alter_system_(solver_parms=list(events=event), 
                  removal_time=at) %>% # include info for later
    alter_parms_(removed_species=to_remove) 
}

# Alter the parameters of a system, non-standard evaluation
alter_parms  <- function(sys, ...) {
  # Get and eval new elems (no pipes here otherwise match.call fails)
  new_elems <- match.call(expand.dots=FALSE)[['...']]
  new_elems <- lapply(new_elems, eval, 
                      envir=sys[["parms"]], enclos=parent.frame())
  
  # Alter the stuff
  sys[["parms"]] <- alter_list_(sys[["parms"]], new_elems=new_elems)
  
  sys
}
# Same function but using standard evaluation
alter_parms_ <- function(sys, ...) {
  sys[["parms"]] <- alter_list_(sys[["parms"]], ...)
  sys 
}

# Alias for alter_system which is more human friendly in pipes
alter_system  <- alter_list
alter_system_ <- alter_list_
