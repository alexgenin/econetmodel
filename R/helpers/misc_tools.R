
# Debugging pipe 
inchain <- function(fun, display.fun=str) { 
  function(...) { 
    browser()
    fun(...)
  }
}

# To be used in pipe chains
chbrowser <- function(passed_object) { 
  browser()
  return(passed_object)
}

# Mapply without simplification
mapply0 <- function(...,SIMPLIFY=FALSE) mapply(..., SIMPLIFY=SIMPLIFY)

# Convert species names to trophic level
sp2tl <- function(spvec) { 
  newvec <- sapply(as.character(spvec),switch,
                    sp5="intermediate",
                    sp6="intermediate",
                    sp7="top",
                    sp8="top",
                    "bottom") # default value
  as.factor(newvec)
}