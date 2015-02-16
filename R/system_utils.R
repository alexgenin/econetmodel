# 
# Some functions that help developing other stuff
# 

generate_cpp_types <- function(parameters) { 
  for (item in parameters) { 
    
    if (is.matrix(item)) { type <- "NumericMatrix" }
    if (is.vector(item)) { type <- "NumericVector" }
    
    cat(paste0(type," ",names(item)," = as<",type,">(p[\"",names(item),"\"]);"),"\n")
  }
}


sqrt_times <- function(from,to,length.out) { 
  stop('Not implemented <!todo!>')
  return(NULL)
}

