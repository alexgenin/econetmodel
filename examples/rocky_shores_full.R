# 
# Full rockyshore model.
# 
# 
# 4 producers
# 2 intermediate consumers
# 2 top predators
# 
# 

Nsp <- 8
spmat <- make.squaremat(Nsp)

# Define some parameters
bodyms <- c(1,1,1,1, 
            10,10,
            100, 100)

# Generate metabolic rates
mr0     <- 0.2227 # ?
mr0exp  <- -.25   # yes !
xs <- gen_met_rates(bodyms, mr0, mr0exp);

# Generate atk rates
a0  <- 27.23734
eps <- 0.01064

# Define topology: who eats whom ?
trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                         list(from=c(7,8), to=c(5,6)))
trophic_topology <- sysmat_fill(trophic_topology, Nsp)
atks <- trophic_topology * gen_atk_rates(bodyms, xs, a0, eps)

# Comsumption rates
ws <- t(apply(trophic_topology, 1, 
              function(X) if (sum(X)>0) X/sum(X) else X))

is_producer <- c(rep(TRUE,4), rep(FALSE,Nsp-4))

parameters <- list(
  # Producers' logistic growth
  # growth rate
  r  = 1 * is_producer,
  # carrying capacities (always > 0 !)
  K  = rep(5,Nsp),
  # Consumption
  # conversion efficiencies
  e  = spmat(0.85), 
  # consumption rates
  w  = ws, 
  # handling times
  h  = 1/(8*xs),
  # metabolic rates (plants have one)
  x  = xs,
  # attack rates (not as !! )
  atk = atks
)

foodweb <- 
  create.system(
    list(time          = 0,
         timestep      = 1,
         tmax          = 1000,
         state         = runif(Nsp, 1, max(parameters$K)),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore_full.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod',
                              dllname  = 'netmodr', 
                              events   = list(func='spkill',
                                              root=TRUE,
                                              terminalroot=2), # max N of eq reaches 0
                              nout=0,
                              rootfunc='controlf',
                              nroot=2)
         ))

foodweb <- compile.system(foodweb) # side-effects !

system.time(
  result.raw <- run(foodweb) 
)

result.df <- export(result.raw)

library(ggplot2)

ggplot(result.df) + 
  geom_line(aes(time, ab, color=sp)) + 
  facet_grid(sp ~ .) + 
  scale_y_sqrt()
  
