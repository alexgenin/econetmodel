
# Dimswitch: returns a different value depending of the number of dimensions
# of an array.
dimswitch <- function(elem, ...) { 
  dots <- as.list(match.call(expand.dots=FALSE))[['...']]
  
  if (is.vector(elem)) {  # if only one dim
    return_n <- 1
  } else {                # more than one dim
    return_n <- length(dim(elem))
  }
  
  if (return_n > length(dots)) { 
    stop("dimswitch: not enough dot arguments provided")
  } else {
    eval(dots[[return_n]], envir=parent.frame())
  }
}


# Create a squared sysmat using a size
sysmat <- function(dat, sys) { 
  matrix(dat,get_size(sys),get_size(sys))
}

rmat <- function(sys, rfun, ...) { 
  matrix(rfun(get_size(sys),...), get_size(sys), get_size(sys))
}

zerodiag <- function(mat) { 
  diag(mat) <- 0
  mat
}

# Fills in a matrix of interactions given a list
# table should be as follow : 
#   [from] [to] [howmuch]
#     a     b       c  
gen_interaction_matrix <- 
  function(links, Nnodes, fill=0, linkval.default=1, 
           mat=matrix(fill, ncol=Nnodes, nrow=Nnodes)) { 
  
  cols <- c('from','to')
  
  for (elem in links) { 
    
    if (is.null(names(elem))) 
      names(elem) <- cols
    
    if ( ! all(cols %in% names(elem)) )
      stop('Bad links specification')
    
    linkval <- if ('val' %in% names(elem)) elem[["val"]] else linkval.default
    
    links.table <- expand.grid(elem[cols])
    for (l in seq.int(nrow(links.table))) { 
      mat[links.table[l,'from'], links.table[l,'to']] <- linkval
    }
  }
  
  return(mat)
}

gim <- function(links, sys, ...) { 
  gen_interaction_matrix(links, get_size(sys), ...)
}

oneof <- function(list) { 
  unlist(sample(list, 1))
}