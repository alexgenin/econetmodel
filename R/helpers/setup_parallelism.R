# 
# 
# This file sets up R for multicore computing. Note that it uses global 
# variables, which is justified here as we mess with the global state.
# 

# Load libraries
library(foreach)
library(doParallel)

# Switch parallelism
.USEPARALLEL <- FALSE
.USENCORES <- 2

# Register a cluster
register <- function() { 
  .LOCALCLUST <<- makeCluster(.USENCORES) # oh my <<- in the global env
  registerDoParallel(.LOCALCLUST) # register parallel backend to foreach
  message("Started local cluster with ", .USENCORES, " cores")
}

# Destroy a cluster
unregister <- function() { 
  stopCluster(.LOCALCLUST)
  rm(.LOCALCLUST,envir=globalenv())
  registerDoSEQ() # register the sequential backend to foreach
  message("Stopped cluster")
}

# Parjob: returns TRUE if conditions are met to run a parallel job. 
parjob <- function() { 
  if ( ! .USEPARALLEL) { 
    return(FALSE);
  } else { 
    
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
    clusterExport(.LOCALCLUST, 
                  varlist=ls(envir=globalenv()),
                  envir=globalenv())
    return(TRUE)
  }
}

# If a cluster exists and we asked for no parallelism, then stop it
if ( ! .USEPARALLEL && exists(".LOCALCLUST") ) { 
  unregister()
# If a cluster does not exist but we asked for parallelism
} else if ( .USEPARALLEL && ! exists(".LOCALCLUST") ) { 
  register()
}
