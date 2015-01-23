# Feeding functions

eat_proportional <- function(amount, who) {
  function(id,X) { 
    X[id] * amount * X[who]
  }
}

eat_proportional_m <- function(amount.mat) { 
  function(id,X) {
    sum(X[id] * X * amount.mat[id, ])
  }
}

