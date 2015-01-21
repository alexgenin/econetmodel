
# Produces a function that gives dx for a logistic growth of parameter r,K.
logistic_growth <- function(r, K) {
  function(id,X) r*X[id]*(1-X[id]/K)
}

