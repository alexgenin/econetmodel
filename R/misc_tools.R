
# Debugging pipe 
inchain <- function(fun, display.fun=str) { 
  function(...) { 
    browser()
    fun(...)
  }
}

# 
chbrowser <- function(passed_object) { 
  browser()
  return(passed_object)
}


mapply0 <- function(...,SIMPLIFY=FALSE) mapply(..., SIMPLIFY=SIMPLIFY)