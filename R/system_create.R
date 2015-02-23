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
                           lib.name = as.character(substitute(system)),
                           lib.dir  = "./src/",
                           CCFLAGS="-Wall") { 
  
  .check_if_system(system)
  
  # Create a temporary directory
  if ( ! file.exists(lib.dir) ) dir.create(lib.dir)
  
  # If already a compiled system, return the compiled system
  if (inherits(system,'system') && inherits(system,'system.binary')) 
    return(system)
  
  # Generate c code
  cfile <- paste0("./src/",lib.name,'.c')
  gen_c_code(get_parms(system), 
             get_template(system),
             output=cfile,
             overwrite=TRUE)
  
  # Compile shared lib
  output <- paste0(lib.dir,"/",sub('.c','',basename(cfile), fixed=TRUE),".so")
  cmd <- paste0('CCFLAGS="',CCFLAGS, '" R CMD SHLIB ',cfile, " -o ",output)
  message(cmd)
  exit_code <- system(cmd)
  
  if (exit_code == 0) { 
    message('Loading shared object.')
    document()
  } else {
    file.remove(cfile)
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

# Checks
.check_if_system <- function(system) { 
  if (!inherits(system,'system')) 
    stop(paste0('I do not know what to do with object of class ', class(system)))
}
