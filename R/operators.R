
# Additive effect
`%p%` <- function(f1,f2) { 
  function(X) {
    f1(X) + f2(X)
  }
}

# Multiplicative effect
`%*%` <- function(f1,f2) { 
  function(id,X) {
    f1(id,X) * f2(id,X)
  }
}

# limit the return value of a function between min and max
limit <- function(f, min=-Inf, max=Inf) {
  function(id,X) {
    min( max(f(id,X),min), max)
  }
}
floor0 <- function(f) limit(f,min=0)
