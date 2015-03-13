# 
# 

# From an interaction matrix, makes a df suitable for use with ggplot's path
ggedge <- function(df, mat, threshold=0) { 
  
  Nedges <- sum(mat > threshold)
  
  output <- data.frame()
  
  cur.edge <- 1
  
  for (i in seq.int(nrow(mat))) { 
    for (j in seq.int(ncol(mat))) { 
      if (mat[i,j] > threshold) { 
        output <- rbind(output, 
                        data.frame(edge = rep(cur.edge, 2), 
                                   x    = c(df[i, 'x'], df[j, 'x']),
                                   y    = c(df[i, 'y'], df[j, 'y'])))
        cur.edge <- cur.edge + 1
      }
    }
  }
  
  return(output)
}
