# 
# Object-oriented stuff for the system class
# 

# Create a system
system_create <- function(list, type='source') { 
  class(list) <- switch(type,
                        source=c('system.source','system','list'))
  return(list)
}

# Compile a system
compile <- function(system,
                    output_cfile = "last_run",
                    lib.dir  = "./src/compiled_systems",
                    include.dir = "./src/include",
                    PKG_CFLAGS="-O2 -Wall -march=native ",
                    quiet=TRUE) { 
  
  .check_if_system(system)
  
  # Get output c file
  output_cfile <- paste0(lib.dir, '/', as.character(output_cfile),'.c')
  
  # Create a temporary directory
  if ( ! file.exists(lib.dir) ) dir.create(lib.dir)
  
  # Generate c code
  gen_c_code(get_parms(system), 
             get_template(system),
             output=output_cfile,
             overwrite=TRUE)
  
  system[["cfile"]] <- output_cfile
  dllname <- sub('.c','',basename(output_cfile), fixed=TRUE)
  system[["solver_parms"]][["dllname"]] <- dllname
  
  # Compile shared lib
  output_so   <- paste0(lib.dir,"/",dllname,".so")
  includes <- paste0(dir(include.dir, pattern=".c$", full.names=TRUE),collapse=' ')
  
  cmd <- paste0('PKG_CFLAGS="', PKG_CFLAGS, ' -I ', include.dir, ' -std=c99" ',
                'R CMD SHLIB ', 
                includes, ' ',
                output_cfile, " -o ", output_so)
  
  message(cmd)
  exit_code <- system(cmd, ignore.stdout=quiet, ignore.stderr=quiet)
  
  if (exit_code == 0) { 
    message('Loading shared object.')
    system[['library']] <- output_so
    dyn.load(output_so)
  } else {
    # file.remove(output_cfile) # usually useful for debugging
    stop('Compilation failed.')
  }
  
  class(system) <- c('system.compiled', 'system', 'list')
  return(system)
}

# These two functions below are not used here but they relate to dealing with 
# c code. They are used in ./R/system_run.R

# Sort the parameter list to make sure they are in the good order in the memory.
# Note that we could put all the non-trophic ones at the end as they are not 
# always used (this way it should be more cache-friendly for the CPU).
prepare_parms <- function(parms, size) {
  
  # Create non-trophic components if not present
  need_nt_component <- ! grepl('^d{1}.*$',names(parms)) # all the ones that begin with d
  need_nt_component <- need_nt_component & 
                         sapply(names(parms), # does not have a d*
                                function(name) { 
                                  ! paste0('d',name) %in% names(parms)
                                })
  
  # defaults to zero for those that are not given already 
  new_components <- lapply(parms[need_nt_component], function(elem) { 
                            array(0, vecmat_switch(elem, 
                                                   rep(length(elem),2), 
                                                   rep(nrow(elem),3))) 
                           })
  names(new_components) <- paste0("d", names(parms)[need_nt_component])
  
  # Return the alphabetically-sorted list with all components. The order 
  # matters as it determines the mapping in memory.
  new_parms <- alter_list_(parms, new_elems=new_components)
  new_parms[order(names(new_parms))]
}

# Handles parameters before passing them to the C code by transforming them into
# a numeric vector. In particular, this allows correct filling of matrices 
# (row-major (C) vs col-major (R))
# 
# TODO: triple-check the in-memory mapping of arrays with ndim >= 3
vectorize_parameters <- function(parms, size) {
  
  parms %>% 
    prepare_parms(size) %>% 
    lapply(function(elem) dimswitch(elem, elem, aperm(elem), aperm(elem))) %>% 
    unlist
}
