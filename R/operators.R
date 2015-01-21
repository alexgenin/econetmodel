
# Additive effect
`%+%` <- function(f1,f2) { 
  function(id,X) {
    f1(id,X) + f2(id,X)
  }
}

# Multiplicative effect
`%*%` <- function(f1,f2) { 
  function(id,X) {
    f1(id,X) * f2(id,X)
  }
}
