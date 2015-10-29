# 
# Generate a trophic_only system
# 
syspreset_rockyshore_brose <- function(tmax = 10e3, timestep = 2, 
        template = "./src/templates/rockyshore.c.template") {
  
  # Set body masses
  # We use a body/mass ratio between prey and predator of 10, which is the mode
  # of its distribution for invertebrates (Brose 2006, Fig. 3).
  bodyms <- c(1,1,1,1, 10, 10, 20, 20)
  Nsp <- length(bodyms)
  
  # We maintain the topology constant here
  trophic_topology <- list(list(from = c(5,6), to = c(1,2,3,4)),
                           list(from = c(7,8), to = c(5,6)))
  trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)
  
  # Compute a default preference
  w <- trophic_topology / ifelse(apply(trophic_topology, 1, sum) > 0,
                                 apply(trophic_topology, 1, sum), 1)
  
  # Topology 
  # Build the parameter list
  parameters <- 
    gen_allometric_system(bodyms, trophic_topology) %>% # x, atk, h
    alter_list(e = matrix(.85, ncol = Nsp, nrow = Nsp), 
               r = c(1,1,1,1, 0,0, 0,0),
               K = rep(1, Nsp),
               q = runif(1,0,1), 
               # Set metabolic rate of producers to zero 
               # as they have mortality includedin logistic growth
               x = x * !(r>0),
               w = w, 
               # These need to be always set to something otherwise C code 
               # does not work
               removed_species = rep(0, Nsp),
               # Competition between species (linear mortality term)
               c = matrix(0, ncol=Nsp, nrow=Nsp),
               # Typical abundance of a species 
               yt = rep(.25, Nsp)) 
  
  # Set time series parameters
  tmin <- 0
  
  # A list of possible removals
  species <- c('algae1','algae2','algae3','algae4',
              'mussels','grazers',
              'whelks','crabs')
  
  system_create(
    list(tmin          = tmin,
         timestep      = timestep,
         tmax          = tmax,
         state         = rep(.5, Nsp),
         parms         = parameters,
         species       = species, 
         source        = list(template = template),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              verbose  = FALSE)))
}

# 
# This file contains stuff to generate systems based on allometry.
# 

# We use flo's manuscript as reference + Sonia's trophic code for atks.
gen_allometric_system <- function(bodyms,
                                  topology,
                                  a0    = 27.23734,
                                  a0exp =  .25,
                                  eps   = 0.01064,
                                  h0    = 0.4,
                                  h0exp = -.75) { 
  
  if ( unique(c(length(bodyms), 
                nrow(topology),  # any different values
                ncol(topology))) != length(bodyms) ) { 
    stop("Parameter sizes mismatch.")
  }
  
  # Coefficients are taken from Brose et al. 2006
  ax <- .314    # Allometric coefficient for metabolism
  ay  <- 8 / ax # Maximum cons. rate coefficient
  expc <- -.25  # Allometric scaling exponent (the same for all, for simplicity)
  
  # Generate metabolic rates (based on Sonia's code)
  # Brose 2006 (eq. 3b)
  xs <- ax * bodyms^expc
  
  # Generate attack rates (based on Sonia's code)  
  atk.rates <- topology
  Nsp <- length(bodyms)
  for (i in seq.int(Nsp)) { 
    atk.rates[i, ] <- topology[i, ] * ay * bodyms[i]^expc
  }
  
  # Generate handling times as in Flo's manuscript
  h0 <- 0.4
  h <- h0*bodyms^h0exp
  
  # Return list
  list(x = xs, atk = atk.rates, h = h)
}
