# 
# This file sets up R for multicore computing. Note that it uses variables in
# its enclosure.
# 
# Parjob: returns TRUE if conditions are met to run a parallel job, and set them
# up if required.
parjob <- function(cores = detectCores()-1, 
                   reset = FALSE, 
                   wd    = getwd()) { 

  # Load libraries
  if ( !require(foreach) || !require(doParallel) ) { 
    stop("parjob requires foreach and doParallel")
  }
  
  # Init variables if non-existent
  if ( ! exists('.LOCALCLUST') ) { 
    .LOCALCLUST <<- list()
  }
  
  # Destroy a cluster
  unregister <- function() { 
    if ( cl_ncores(.LOCALCLUST) >= 2) stopCluster(.LOCALCLUST)
    .LOCALCLUST <<- list()
    registerDoSEQ() # register the sequential backend to foreach
    message("Stopped cluster")
  }
  
  # Register a cluster
  register <- function() { 
    .LOCALCLUST <<- makeCluster(cores) # oh my <<- in the closure env
    registerDoParallel(.LOCALCLUST) # register parallel backend to foreach
    message("Started local cluster with ", cores, " cores")
  }
  
  # Test whether cluster is on
  cl_ncores <- function(cl) length(cl)
  
  # Test whether cluster is functional or whether it produces errors
  if ( cl_ncores(.LOCALCLUST) >= 2 ) {
    test_result <- try(clusterEvalQ(.LOCALCLUST, TRUE), silent=TRUE)
    if ( inherits(test_result,"try-error") ) { 
      message("Shutting down broken cluster")
      
      slaves <- system('pgrep -f -u "$USER" "/usr/lib/R/bin/exec/R --slave"', 
                       intern = TRUE)
      for (slave in slaves) { 
        message('Killing process ', slave)
        system(paste("kill",slave))
      }
      closeAllConnections()
      .LOCALCLUST <<- list()
    }
  }
  
  # Operating part 
  # -----------------------------------------
  
  if (reset && cl_ncores(.LOCALCLUST) >= 2) unregister()
  
  # Bail if non-parallel computation, but keep cluster status unchanged
  if ( cores <= 1 ) { 
    return(FALSE);
  } else { 
    
    # We reset if we changed the number of cores 
    if ( cl_ncores(.LOCALCLUST) != cores ) { 
      unregister()
      register()
    }
    
    # Get to current directory in workers
    clusterCall(.LOCALCLUST, setwd, wd)
    
    # Reload current directory in workers
    clusterCall(.LOCALCLUST, document)
    
    # Export packages
    pkgs <- .packages()
    for (pkg in pkgs) { 
      clusterCall(.LOCALCLUST, library, pkg, character.only=TRUE)
    }
    
    # Export environments
    clusterExport(.LOCALCLUST, 
                  varlist=ls(envir=parent.frame()),
                  envir=parent.frame())
    
    return(TRUE)
  }
}
