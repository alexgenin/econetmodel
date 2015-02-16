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

size <- 5
sysmat <- function(vec) matrix(vec, ncol=size, nrow=size, byrow=TRUE)

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
atks <- gen_atk_rates(bodyms, xs, a0, eps) * atks.qualitative

# Comsumption rates
ws <- sysmat(c(0,   0,  0, 0, 0,
               .25, 0,  0, 0, 0,
               .25, 0,  0, 0, 0,
               0,  .1, .1, 0, 0,
               0,  .1, .1, 0, 0))

parameters <- list(
  # Producers' logistic growth
  # growth rate
    rs  = c(1, rep(0,size-1)), 
    # carrying capacities
    Ks  = rep(10, size),
  # Consumption
    # consumption rates
    ws  = ws, 
    # conversion efficiencies
    es  = sysmat(0.85), 
    # handling times
    hs  = 1/(8*xs),                   
    # metabolic rates (plants have one)
    xs  = xs,
    # attack rates (not as as it is a reserved keyword in cpp and we want to be consistent)
    atks = atks
)

init <- runif(size, .1, 10)
system <- create_system(rockyshore, 
                        init_time=0, 
                        tres=1,
                        init_state=init, 
                        parms=parameters) 

# test
with(system, func(time, as.vector(state[ ,-1]), parms)) 

# Run da stuff
ii <- profr(
  result <- ddply(data.frame(N=seq.int(50)), ~N, 
                  function(x) {
                    Sys.sleep(1)
                    system %>% 
                      state_alter(runif(size,.1,10)) %>%
                      run(1000) %>%
                      remove_species(5) %>%
                      run(1000) %>%
                      .[['state']]
                  },
                  .progress='time'),
                  0.01)


# Format and plot
library(tidyr) 
library(ggplot2)

plot.dat <- gather(as.data.frame(result), sp, ab, node1:node5)

ggplot(subset(plot.dat)) + 
  geom_line(aes(time, ab, color=sp, group=N), alpha=.2) + 
#   geom_point(aes(time, ab, color=sp)) + 
  facet_grid(sp~.) +
  scale_x_sqrt() 
#   ylim(c(0,3))

