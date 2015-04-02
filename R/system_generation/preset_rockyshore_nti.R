# 
# Generate a trophic_only system
# 
syspreset_rockyshore_nti <- function(tmax) { 
  bodyms <- c(1,1,1,1, 3,3, 6,6)
  Nsp <- length(bodyms)
  
  parameters <- alter_list(default_trophic_parms(bodyms), 
                           # We default to not removing species but these parameters
                           # need to be set so the C code works correctly.
                           removed_species_total=0,
                           removed_species=0)
  
  # We set the metabolic rate of producers to zero so they have no intrinsic 
  # mortality
  parameters <- parameters %>% 
                  alter_list(x=c(0,0,0,0, parameters[['x']][5:Nsp])) %>% 
                  alter_list(nt=matrix(0,Nsp,Nsp))
  
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
         source        = list(template = './src/templates/rockyshore.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
#                               nout     = 1,
#                               outnames = "extinct",
#                               rootfunc = 'controlf',
#                               nroot    = 2,
                              verbose  = FALSE)))
}
