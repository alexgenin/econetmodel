# 
# Draft for a rocky shore model
# 

# Needs rockyshore model
# 5 species
#   1 algae
#   2 grazers
#   3 mussels
#   4 whelks
#   5 crabs

Nsp <- 5
sysmat <- function(vec) matrix(vec, ncol=Nsp, nrow=Nsp, byrow=TRUE)

# Define some parameters
bodyms <- c(1,10,10,100,100)

# Generate metabolic rates
mr0     <- 0.2227 # ?
mr0exp  <- -.25   # yes !
xs <- gen_met_rates(bodyms, mr0, mr0exp);

# Generate atk rates
a0  <- 27.23734
eps <- 0.01064
atks.qualitative <- sysmat(c(0,0,0,0,0,  # algae
                             1,0,0,0,0,  # grz1
                             1,0,0,0,0,  # grz2
                             0,1,1,0,0,  # tp1
                             0,1,1,0,0)) # tp2
# atks <- gen_atk_rates(bodyms, xs, a0, eps) * atks.qualitative
atks <- gen_atk_rates(bodyms, xs, a0, eps) * atks.qualitative

# Comsumption rates
ws <- sysmat(c(0,  0,  0, 0, 0,
               1,  0,  0, 0, 0,
               1,  0,  0, 0, 0,
               0, .5, .5, 0, 0,
               0, .5, .5, 0, 0))

parameters <- list(
  # Producers' logistic growth
  # growth rate
  r  = c(1, rep(0,Nsp-1)), 
  # carrying capacities
  K  = rep(1, Nsp),
  # Consumption
  # conversion efficiencies
  e  = sysmat(0.85), 
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
         tmax          = 10e5,
         state         = abs(runif(5)),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod',
                              dllname  = 'netmodr', 
                              events   = list(func='spkill',
                                              root=TRUE,
                                              terminalroot=2), # when max N of eq reaches 0
                              nout=0,
                              rootfunc='equilibrium',
                              nroot=2)
         ))

foodweb <- compile.system(foodweb) # side-effects !
document()

system.time(
  result <- run(foodweb, tmax=10e5) %>% export
)

library(ggplot2)
ggplot(result) +
  geom_line(aes(time, ab, color=sp)) + 
  facet_grid(sp~.)
