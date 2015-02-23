
# A structure for a system type

# Does nothing when document() is called but we have syntax colors in kate so 
# its good
quote(
  system <- list( 
      times         = double,
      timestep      = double, 
      state         = double[],
      parms         = list(double [], ...), # only doubles !
      source        = list(template = char[]),
      solver_parms  = list(parms    = double[], # these are parameters that are accepted by runsteady
                           ...), # all kinds of stuff
      stability_status = int)
)

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
get_time             <- get_in_list ('time')
get_timestep         <- get_in_list ('timestep')
get_state            <- get_in_list ('state')
get_size             <- function(list) length(get_in_list('state')(list))
get_parms            <- get_in_list ('parms')
get_solver_parms     <- get_in_list ('solver_parms')
get_template         <- get_in_list2('source','template')
get_stability_status <- get_in_list ('stability_status')

# Set the compiled form for parameters
set_compiled_parms <- function(system,parms) { 
  system[['solver_parms']][['parms']] <- parms
  system
}

