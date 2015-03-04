# 
# Some checking function for systems objects



# Creates a checking function for a given class
class_checker <- function(class, error=TRUE) { 
  function(obj) { 
    if (!inherits(obj,class)) {
      stop(paste0('I do not know what to do with object of class ', 
                  class(system)))
    }
    return(inherits(obj,class))
  }
}

.check_if_system <- class_checker('system', error=TRUE)