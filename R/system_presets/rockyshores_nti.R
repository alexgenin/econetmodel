# 
# Generate a trophic_only system
# 
syspreset_rockyshore_nti <- function(tmax, 
                                     remove_species=FALSE) { 
  Nsp <- 8
  
  # Generate K and dK
  dK <- list(list(from=5, to=c(1,2,3,4), val=.3))
  dK <- gen_interaction_matrix(dK, Nsp)
  
  parameters <- alter_list_(default_trophic_parms(), 
                              list(
                                # NT bonus/malus on carrying capacity
                                dK = dK,
                                # Species to remove
                                removed_species = 5
                              )
                            )
  
  # Set time series parameters
  time <- 0 
  timestep <- 2
  removal_time <- 3000
  
  event <- NULL
  if (remove_species) { 
    event <- list(func = 'remove_species',
                  root = FALSE,
                  time = nearestEvent(removal_time, 
                                      seq(0,tmax,by=timestep))) # the root max nb of eq 
  }
  
  create.system(
    list(time          = time,
         timestep      = timestep,
         tmax          = tmax,
         removal_time  = removal_time,
         state         = rep(.5, Nsp),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore_nti.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              events   = event,
#                               nout     = 1,
#                               outnames = "extinct",
#                               rootfunc = 'controlf',
#                               nroot    = 2,
                              verbose  = FALSE)
         ))
  
}