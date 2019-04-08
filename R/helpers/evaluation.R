# Match the dots of a function and return it as an eval'ed list
match_dots_and_eval <- function(..., envir=parent.frame(), enclos=NULL) { 
  dots <- match.call(expand.dots=FALSE)[['...']]
  return(lapply(dots, eval, envir=envir, enclos=enclos))
} 

# Convert a function that takes an explicit list as second argument to a 
# function that takes dots instead.
make_dotted <- function(fun) {
  function(obj, ...) {
      fun(obj, match_dots_and_eval(...))
  }
}
