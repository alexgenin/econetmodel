# 
# Full rockyshore model.
#
library(deSolve)
library(magrittr)
library(ggplot2)
library(plyr)

# 
# 4 producers
# 2 intermediate consumers
# 2 top predators
# 
# 

# Define some parameters
bodyms <- c(1,1,1,1,  10,10,  100,100)
Nsp <- length(bodyms)

# Define topology: who eats whom ?
trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                         list(from=c(7,8), to=c(5,6)))
trophic_topology <- gen_interaction_matrix(trophic_topology, Nsp)

# Generate 
allometric_vars <- gen_allometric_system(bodyms, trophic_topology)

# Typical abundances of each
ab0 <- rep(.5,Nsp)

# NTI: mussels (6) create space for algaes to grow on (K++)
NTI_topology_mussels <- list(list(from=6, to=c(1,2,3,4)))
NTI_topology_mussels <- gen_interaction_matrix(NTI_topology_mussels, Nsp)
NTI_mussels <- NTI_mult("K", NTI_topology_mussels, 1, 1.5)

# Comsumption rates
ws <- t(apply(trophic_topology, 1, 
              function(X) if (sum(X)>0) X/sum(X) else X))


parameters <- alter_list(allometric_vars, 
                         NTI_mussels,
                         list(
                           # Typical abundances
                            ab0  = rep(.5, Nsp),
                           # Producers' logistic growth
                            # growth rate
                            r  = c(rep(1,4), rep(0,Nsp-4)),
                           # Consumption
                            # conversion efficiencies
                            e  = matrix(0.85, ncol=Nsp, nrow=Nsp), 
                            # consumption rates
                            w  = ws, 
                           # Operations on system 
                            # Removed species
                            removed_species = 4
                           ))

# Set time series parameters
time <- 0 
timestep <- 4
tmax <- 2e3L

foodweb <- 
  create.system(
    list(time          = time,
         timestep      = timestep,
         tmax          = tmax,
         state         = rep(.5,Nsp),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              events   = list(func = 'remove_species',
                                              root = FALSE,
                                              time = nearestEvent(2500, seq(0,tmax,by=timestep)),
                                              terminalroot=2), # the root max nb of eq 
                              nout     = 2,
                              outnames = c("maxd","remaining_eq"),
                              rootfunc = 'controlf',
                              nroot    = 2,
                              verbose  = FALSE)
         ))

foodweb.c <- compile.system(foodweb) # side-effects !

# Define what one run is supposed to be
do_onerun <- function(rundat, foodweb) { 
  alter_system(foodweb, list(state=runif(Nsp,.001, 1))) %>% 
    run %>% 
    export(gather.do=TRUE) 
}

system.time({ 
  rundat <- data.frame(id=seq.int(4))
  result <- ddply(rundat, names(rundat), 
                  do_onerun, foodweb.c, 
                  .progress='time')
})

# Export results
library(ggplot2)
result.plot <- ggplot(result) + 
                 geom_line(aes(time, ab, group=id), 
                           alpha = .2, size = .7) + 
                 facet_grid(sp ~ id)
print(result.plot)
# ggsave(plot=result.plot, filename="../output/plots/draft/test.png")

