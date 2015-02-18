
# 
compile_load <- function(file) { 
  
  file.base <- sub('.c','',basename(file), fixed=TRUE)
  
  system(paste('CCFLAGS="-Wall" R CMD SHLIB', file))
  dyn.load(paste0(dirname(file),'/',file.base,'.so'))
  
  return(NULL)
}

# ode(y=rnorm(3), 
#     times=seq.int(1000), 
#     func='derivs', 
#     initfunc='initmod',
#     parms=rnorm(3), 
#     dllname='rockyshore_compiled', 
#     nout=3)
