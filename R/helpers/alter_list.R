# 
# List-alteration functions. All these functions take a list as first argument
# and add/replace components as provided. Some of them use an explicit list
# to specify modifications, and some of them use non-standard evaluation (NSE).
# 
# alter_list_nodots_ SE  + explicit list for modifs
# alter_list_nodots  NSE + explicit list for modifs
# alter_list_        SE  + dots for modifs
# alter_list         NSE + dots for modifs
#


# Alter a list: standard evaluation 
alter_list_ <- function(list, ..., new_elems=NULL) { 
  
  # If unspecified new_elems, then parse the dots
  if (is.null(new_elems)) { 
    new_elems <- match.call(expand.dots=FALSE)[["..."]] 
    new_elems <- lapply(new_elems, eval, envir=parent.frame())
  }
  
  # Sanity check
  if (is.null(names(new_elems))) stop("No names found in list.")
  
  for (name in names(new_elems)) { 
    
    # If both args for this name are lists, we recurse.
    # NB: Note that new_elems is evaluated in the context of the first leval of 
    # list.
    if ( is.list(list[[name]]) && is.list(new_elems[[name]])) { 
      list[[name]] <- alter_list_(list[[name]], 
                                  new_elems=new_elems[[name]])
      
    } else { 
      list[[name]] <- new_elems[[name]]
    }
  }
  
  return(list)
}

# Alter a list: non-standard evaluation
alter_list <- function(list, ..., 
                       new_elems=NULL, 
                       default_env=parent.frame()) { 
  
  # If new_elems is not passed explicitely, then we loop over the dots.
  if (is.null(new_elems)) { 
    new_elems <- match.call(expand.dots=FALSE)[["..."]]
  }
  
  # Sanity check
  if (is.null(names(new_elems))) stop("No names found in list.")
  
  for (name in names(new_elems)) { 
    new_elem <- new_elems[[name]]
    if ( is.list(list[[name]]) && # FALSE if non-existent
         is_unevaled_list(as.expression(new_elem)) ) { 
       # We recall alter_list with explicit new_elems new. It needs to be 
       # formatted beforehand though by removing the list() part of the call.
       list[[name]] <- alter_list(list[[name]], 
                                  new_elems=as.list(new_elem)[-1],
                                  default_env=default_env)
                          
    } else { 
      list[[name]] <- eval(new_elem, 
                           envir=list, 
                           enclos=default_env)
    }
    
  }
  
  return(list)
}

# Examples:
# alter_list_(list, lazy_dots(...))
# alter_list_nodots(flatlist, list(a=10*b))
# alter_list_nodots(deeplist, list(z=list(a=10*b, b=-1)))
# alter_list(deeplist, z=list(a=10*b, b=runif(1,-1,1)))

is_unevaled_list <- function(stuff) grepl("list\\(.*\\)$", as.character(stuff))
