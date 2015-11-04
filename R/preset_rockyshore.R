# 
# Generate a trophic_only system
# 
syspreset_rockyshore <- function(tmax = 10e3, timestep = 2, 
        template = "./src/templates/rockyshore.c.template") {
  
  # Set body masses
  # We use a body/mass ratio between prey and predator of 10, which is the mode
  # of its distribution for invertebrates (Brose 2006, Fig. 3).
  bodyms <- c(1,1,1,1, 10, 10, 100, 100) 
  is_basal_species <- bodyms == 1
  Nsp <- length(bodyms)
  
  # We maintain the topology constant here
  trophic_topology <- list(list(from = c(5,6), to = c(1,2,3,4)),
                           list(from = c(7,8), to = c(5,6)))
  trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)
  
  # Compute a default preference
  w <- trophic_topology / ifelse(apply(trophic_topology, 1, sum) > 0,
                                 apply(trophic_topology, 1, sum), 1)
  
  delta <- .15
  
  # Topology 
  # Build the parameter list
  parameters <- 
    gen_allometric_yodzis(bodyms, trophic_topology, 
                          delta = delta,
                          basal_species = is_basal_species) %>% # x, y, h
    alter_list(K = rep(1, Nsp),
               q = runif(1,0,1), 
               # Set metabolic rate of producers to zero 
               # as they have mortality included in logistic growth
               x = x * !is_basal_species,
               w = w, 
               # Fraction of food lost when eating
               delta = delta,
               # Fraction of eaten food actually metabolized
               e = matrix(1, ncol = Nsp, nrow = Nsp), 
               # These need to be always set to something otherwise C code 
               # does not work
               removed_species = rep(0, Nsp),
               # Competition between species (linear mortality term)
               c = matrix(0, ncol = Nsp, nrow = Nsp),
               # Typical abundance of a species 
               yt = rep(.25, Nsp)) 
  
  # Compute attack rates
  a0 <- 0.01
  atk <- trophic_topology
  for (i in seq_along(bodyms)) {
    for (j in seq_along(bodyms)) {
      atk[i, j] <- trophic_topology[i,j] * 
                     a0 * bodyms[i] ^ (0.25) * bodyms[j] ^ (0.25) 
#       atk[i,j] <- trophic_topology[i,j] * 
#                     parameters[["x"]][i] * exp(-eps * bodyms[i] / bodyms[j])
    }
  }
  
  parameters <- alter_list(parameters, atk = atk)
  
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

gen_allometric_yodzis <- function(bodyms, trophic_topology, 
                                  at = 0.314, # metab rate scaling, ax in Brose, 2006
                                  ar = 1, # r rate scaling
                                  aj = 8*at, # max cons rate scaling (ay in Brose)
                                  fr = 1, # coefficient for r scaling
                                  fj = 1, # coefficient for m. a. r. scaling
                                  basal_species = 1,
                                  delta = 0) # proportion lost of ingested food
                                  { 
  
  # The xs depend on the mass of the basal species, assumed to be 
  # on the first slot
  xs <- ( at / (fr*ar) ) * ( bodyms / mean(bodyms[basal_species]) )^-0.25
  
  # The ys are a constant that depend only on allometric parameters
  ys <- (fj * aj) / at # == aj for default params
  
  # Handling times
  hs <- (1 - delta) / ( fj * aj * bodyms^-0.25 )
  
  # Return the list of parameters
  list(x = xs, h = hs, y = ys)
  
}
