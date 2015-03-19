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
  .LOCALCLUST <<- makeCluster(.USENCORES) # oh my <<-
  registerDoParallel(.LOCALCLUST) # register parallel backend to foreach
  message("Started cluster with ", .USENCORES, " cores")
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
    # Export packages
    pkgs <- .packages()
    sapply(pkgs, function(p) clusterEvalQ(.LOCALCLUST,
                                          library(p, character.only=TRUE)))
    
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

# If a cluster exists, then stop it
if ( exists(".LOCALCLUST") ) { 
  unregister()
}

# Create a new cluster if necesary
if (.USEPARALLEL) register()

