# 
# Creates a system function given some inline cpp code
# 


cpp <- function(name, body, print=TRUE) { 
  require(Rcpp)
  
  # Header of cpp function: we decrement id as indexing starts with a 
  # 1 in R, but 0 in C++
  header <- c(paste0('double ',name,'(int id, NumericVector X) { '),
              'double out; ',
              'id--;') 
  
  # Build body. 
  body <- paste0('out = ', body,';')
  
  footer <- c('return out;',' }')
  
  if (print) { 
    for (line in c(header,body,footer)) cat(line,'\n')
  }
  
  ftext <- paste(paste0(header,collapse=' '), 
                 paste0(body,  collapse=' '), 
                 paste0(footer,collapse=' '))
  ftext
}
