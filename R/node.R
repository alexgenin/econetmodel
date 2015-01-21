# 
# Makes a proper function suitable for ode

node <- function(id,sysfun) {
  function(state) { 
    list(sysfun(id,state))
  }
}

