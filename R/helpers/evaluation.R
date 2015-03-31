# Given a function that receives a list as second arguments f(obj,list), 
# returns the same function, but using "..." as the list argument f(obj,...).
make_dotted <- function(fun) { 
  function(obj, ...) { 
    fun(obj, match_dots_and_eval(...))
  }
}

# Match the dots of a function and return it as an eval'ed list
match_dots_and_eval <- function(...) { 
  dots <- match.call(expand.dots=FALSE)[['...']]
  return(lapply(dots, eval, envir=parent.frame()))
} 

