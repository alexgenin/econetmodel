
# A structure for a system type

# Generic function to retrive stuff from a list or NULL !
get_in_list <- function(name) { 
  function(system) { 
    system[[name]]
  }
}

# Generic function to retrive stuff from the second level of a list or NULL !
get_in_list2 <- function(name1,name2) { 
  function(system) { 
    system[[name1]][[name2]]
  }
}

# Define the functions to access a system's data
get_tmin             <- get_in_list ('tmin')
get_timestep         <- get_in_list ('timestep')
get_tmax             <- get_in_list ('tmax')
get_rtime            <- get_in_list ('removal_time')
get_parms            <- get_in_list ('parms')
get_state            <- get_in_list ('state')
get_size             <- function(list) length(get_in_list('state')(list))
get_solver_parms     <- get_in_list ('solver_parms')
get_template         <- get_in_list2('source','template')
get_stability_status <- get_in_list ('stability_status')
get_layout           <- get_in_list('layout')
get_species          <- get_in_list('species')
