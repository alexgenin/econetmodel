# 
# Generate a trophic_only system
# 
syspreset_rockyshore <- function(tmax, remove_species=FALSE) { 
  
  # Define some parameters
  bodyms <- c(1,1,1,1,  3,3,  6,6)
  Nsp <- length(bodyms)

  # Define topology: who eats whom ?
  trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                           list(from=c(7,8), to=c(5,6)))
  trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)
  
  # Generate 
  allometric_vars <- gen_allometric_system(bodyms, trophic_topology)
  
  # Comsumption rates
  ws <- t(apply(trophic_topology, 1, 
                function(X) if (sum(X)>0) X/sum(X) else X))

  parameters <- alter_list(allometric_vars, 
                            c(list(
                              # Producers' logistic growth
                                # growth rate
                                r  = c(rep(1,4), rep(0,Nsp-4)),
                                # carrying capacities (always > 0 !)
                                K  = rep(1, Nsp),
                              # Consumption
                                # conversion efficiencies
                                e  = matrix(0.85, ncol=Nsp, nrow=Nsp), 
                                # consumption rates
                                w  = ws,
                              # Functional response [0,1]
                                q  = .5), 
                              list(
                              # Species to remove
                                removed_species = 5
                              )
                            ))
  
  # Set time series parameters
  time <- 0 
  timestep <- 1
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
         kept_output   = list(a.init    = c(0, 0),
                              b.before  = c(removal_time - 200 * timestep, removal_time),
                              c.removed = c(tmax - 200, tmax)),
         state         = rep(max(parameters$K), Nsp),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore_trophiconly.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              events   = event,
                              nout     = 1,
                              outnames = c("extinct"),
#                               maxstep = 10e6, # non-default value not recommended
#                               rootfunc = 'controlf',
#                               nroot    = 2,
                              verbose  = FALSE)
         ))
  
}