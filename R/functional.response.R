
# This is a classic functional response à la Kéfi2012.
# Given a parameter K it modifies as to make it depend on the biomass of 
# another node. 
# K => ( (Knti * x) + mX0 ) / (x + X0)
# 
# We consider in the definition that P is the parameter of concern
fresp_kefi <- function(Pnti, P0, N0) { 
  function(id, X) {
    sum( (Pnti[id, ]*X + P0[id, ]*N0[id, ]) / (X + N0[id, ]) )
  }
}
