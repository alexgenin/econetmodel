# 
# Returns a set of default parameters
# 

default_trophic_parms <- function(...) { 
  
  # Body masses and number of species
  bodyms <- c(1,1,1,1, 3,3, 6,6)
  Nsp <- length(bodyms)
  
  # Define topology: who eats whom ?
  trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                           list(from=c(7,8), to=c(5,6)))
  trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)
  
  # Generate 
  allometric_vars <- gen_allometric_system(bodyms, trophic_topology)
  
  # Generate K and dK
  K0 <- rep(1,Nsp)
  
  # Comsumption rates
  ws <- t(apply(trophic_topology, 1, 
                function(X) if (sum(X)>0) X/sum(X) else X))

  # Typical abundances of species
  y0 <- rep(.5, Nsp)
  
  # Conversion efficiencies
  e <- matrix(.85, ncol=Nsp, nrow=Nsp)
  
  # Default reproduction rate
  r <- c(1,1,1,1, 0,0, 0,0)
  
  alter_list(allometric_vars,
             r=r,
             K0=K0,
             y0=y0,
             e=e,
             w=ws,
             q=.5) 
}