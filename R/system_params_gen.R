# Misc functions to generate parameters


# Given a number, provide a wrapper to a matrix of the right size
make.squaremat <- function(Nnodes) { 
  function(vec) { 
    matrix(vec, ncol=Nnodes, nrow=Nnodes, byrow=TRUE)
  }
}

# Fills in a matrix of interactions given a table
# table should be as follow : 
#   [from] [to] [howmuch]
#     a     b       c  
sysmat_fill <- function(links, Nnodes, fill=0, linkval.default=1, 
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

