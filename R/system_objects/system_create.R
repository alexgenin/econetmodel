# 
# Object-oriented stuff for the system class
# 

# Create a system
create.system <- function(list, type='source') { 
  class(list) <- switch(type,
                        source=c('system.source','system','list'))
  return(list)
}

# Compile a system
compile.system <- function(system, 
                           output_cfile = "last_run",
                           lib.dir  = "./src/compiled_systems",
                           include.dir = "./src/include",
                           PKG_CFLAGS="-Wall") { 
  
  .check_if_system(system)
  
  # Get output c file
  output_cfile <- paste0(lib.dir,'/',as.character(output_cfile),'.c')
  
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
  
  cmd <- paste0('PKG_CFLAGS="',PKG_CFLAGS, ' -I ',include.dir,'"',
                ' R CMD SHLIB ', 
                includes, ' ',
                output_cfile, " -o ", output_so)
  message(cmd)
  exit_code <- system(cmd)
  
  if (exit_code == 0) { 
    message('Loading shared object.')
    dyn.load(output_so)
  } else {
    # file.remove(output_cfile)
    stop('Compilation failed.')
  }
  
  # Process parameters in a way the compiled code can understand
  system <- set_compiled_parms(system,
                               prepare_parameters(get_parms(system)))
  
  class(system) <- c('system.compiled', 'system', 'list')
  return(system)
}

# Handles parameters before passing them to the C code by transforming them into
# a numeric vector. In particular, this allows correct filling of matrices 
# (row-major (C) vs col-major (R))
prepare_parameters <- function(parameters) { 
  lapply(parameters, 
         function(elem) vecmat_switch(elem, elem, as.vector(t(elem)))) %>%
  unlist
}
