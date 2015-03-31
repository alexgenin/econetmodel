# 
# Generate a trophic_only system
# 
syspreset_rockyshore_nti <- function(tmax) { 
  bodyms <- c(1,1,1,1, 3,3, 6,6)
  Nsp <- length(bodyms)
  
  # Generate K and dK
  dK <- list(list(from=5, to=c(1,2,3,4), val=.5))
  dK <- gen_interaction_matrix(dK, Nsp)
  
  parameters <- alter_list(default_trophic_parms(bodyms), 
                           dK = dK,
                           # We default to not removing species but these parameters
                           # need to be set so the C code works
                           removed_species_total=0,
                           removed_species=0)
  
  # Set time series parameters
  time <- 0 
  timestep <- 2
  removal_time <- 3000
  
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
#                               nout     = 1,
#                               outnames = "extinct",
#                               rootfunc = 'controlf',
#                               nroot    = 2,
                              verbose  = FALSE)
         ))
  
}
