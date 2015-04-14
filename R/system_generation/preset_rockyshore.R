# 
# Generate a trophic_only system
# 
syspreset_rockyshore <- function(tmax=10e3, timestep=2) {
  
  # Set body masses
  bodyms <- c(1,1,1,1, 3,3, 6,6)
  Nsp <- length(bodyms)
  
  # We maintain the topology constant here
  trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                           list(from=c(7,8), to=c(5,6)))
  trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)

  # Topology 
  
  # Build the parameter list
  parameters <- 
    gen_allometric_system(bodyms, trophic_topology) %>% # x, atk, h
    alter_list(e = matrix(.85, ncol=Nsp, nrow=Nsp), 
               r = c(1,1,1,1, 0,0, 0,0),
               K = rep(1,8),
               q = runif(1,0,1), 
               # Equal value of w for each prey
               w = vegan::decostand(trophic_topology, "total", 1),
               # Set metabolic rate of producers to zero 
               # as they have mortality includedin logistic growth
               x=c(0,0,0,0, x[5:Nsp]),
               # These need to be always set to something otherwise C code 
               # does not work
               removed_species=rep(0,Nsp),
               # Typical abundance of a species 
               yt=rep(1/2,Nsp)) 
  
  # Set time series parameters
  tmin <- 0 
  
  system_create(
    list(tmin          = tmin,
         timestep      = timestep,
         tmax          = tmax,
         state         = rep(.5, Nsp),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              verbose  = FALSE)))
}
