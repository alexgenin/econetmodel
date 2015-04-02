# 
# 

# Read the edges list from a system, SE and NSE version
links_ <- function(sys, parm, layout, threshold = 0) { 
  as_edges(layout, get_parms(sys)[[parm]], threshold = threshold)
}
links <- function(sys, parm, layout, threshold = 0) { 
  parm <- eval(substitute(parm), envir=get_parms(sys), enclos=parent.frame())
  as_edges(layout, parm, threshold = threshold)
}

# From an interaction matrix, makes a df suitable for use with ggplot's path
as_edges <- function(xy, mat, threshold=0) { 
  
  Nedges <- sum(mat > threshold)
  
  output <- as.data.frame(matrix(NA, ncol=6))
  names(output) <- c('edge', 'x','xend','y','yend','value')
  
  cur.edge <- 1
  
  for (i in seq.int(nrow(mat))) { 
    for (j in seq.int(ncol(mat))) { 
      if (abs(mat[i,j]) > threshold) { 
        output[cur.edge, ] <- 
          data.frame(edge  = cur.edge, 
                     x     = xy[i, 'x'], 
                     xend  = xy[i, 'x'] + (xy[j, 'x'] - xy[i, 'x']),
                     y     = xy[i, 'y'], 
                     yend  = xy[i, 'y'] + (xy[j, 'y'] - xy[i, 'y']),
                     value = mat[i,j])
        cur.edge <- cur.edge + 1
      }
    }
  }
  
  return(output)
}
