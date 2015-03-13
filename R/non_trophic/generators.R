
# Helpers to generate non-trophic interactions

NTI_mult <- function(name.skel, topology, I0, NTI_factor) { 
  
  Nsp <- ncol(topology)
  
  # The values without NTI
  mat_I0   <- matrix(I0, ncol=Nsp, nrow=Nsp) 
  
  # The values with NTI @ full capacity
  mat_INTI <- ifelse(topology > 0, mat_I0 * NTI_factor, mat_I0)
  
  # Format and return
  out <- list(mat_I0, mat_INTI)
  names(out) <- paste0(name.skel, c('0','NTI'))
  
  return(out)
}