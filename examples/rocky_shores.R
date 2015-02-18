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
               .4, 0,  0, 0, 0,
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

parameters.c <- prepare_parameters(parameters)

# Compile and load c code
clean_c_code("./src/templates/")
gen_c_code(parameters, "./src/templates/rockyshore.c.template", overwrite=TRUE)
document() # loads .so too

# Define run variables
times      <- seq(0,1000,l=3000)
parms      <- parameters.c
init.state <- rnorm(Nsp,.5,.1)
names(init.state) <- paste0('node',seq.int(Nsp))
events     <- list(func='killspecies', 
                   time=nearestEvent(500,times))

system <- list(y=init.state,
               times=times,
               parms=parms,
               func='rockyshore', 
               initfun="initmod",
               dllname='netmodr', 
               events=list(func='spkill',
                           time=nearestEvent(500,times)),
               nout=1,
               outnames='total',
               method='lsoda')

pipeline <- function(x) { 
  system %>% 
    alter(y=runif(Nsp, 0, 1)) %>%
    do.call(ode,.) %>% 
    cbind(x,.)
}

system.time(
  result.raw <- lapply(as.list(seq.int(100)), pipeline) 
)

# Format output 
result <- do.call(rbind, result.raw)
# result <- result[ ,-ncol(result)]
colnames(result) <- c('N','time',paste0('node',seq.int(Nsp)),'total')

# Format and plot
library(tidyr)
library(ggplot2)

plot.dat <- gather(as.data.frame(result), sp, ab, node1:node5)

ggplot(subset(plot.dat)) + 
  geom_line(aes(time, ab, group=N), alpha=.1) + 
#   geom_point(aes(time, ab, color=sp)) + 
  facet_grid(sp~.) 
#   ylim(c(0,3))

