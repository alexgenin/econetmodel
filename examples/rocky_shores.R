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
sysmat <- function(vec) matrix(vec, ncol=size, nrow=size)

ats.qualitative <- sysmat(c(0, 0, 0, 0, 0,
                            1, 0, 0, 0, 0,
                            1, 0, 0, 0, 0,
                            0, 1, 1, 0, 0,
                            0, 1, 1, 0, 0))
ats.quantitative <- ats.qualitative * sysmat(abs(rnorm(size^2, 0, .1)))

parameters <- list(
  # Producers' logistic growth
    # growth rateà
    rs  = c(.01, rep(0,size-1)),              
    # carrying capacities
    Ks  = rep(10, size),                  
  # Consumption
    # attack rates (not as as it is a reserved keyword in cpp)
    ats = ats.quantitative, 
    # consumption rates
    ws  = sysmat(.2), 
    # conversion efficiencies
    es  = sysmat(1), 
    # handling times
    hs  = rep(1,size),                   
    # metabolic rate
    xs  = rep(.1,size)                   
  )

init <- runif(size,.1,10)
system <- create_system(rockyshore, 
                        init_time=0, 
                        init_state=init, 
                        parms=parameters) 

# test
with(system, func(time, as.vector(state[ ,-1]), parms)) 

# Run da stuff
result <- ddply(data.frame(N=seq.int(200)), ~ N, 
                function(dat) { 
                  system %>%
                    run(times=200) %>%
                    remove_species(2) %>%
                    run_to_eq(tmax=2000) %>%
                    .[['state']]
                },
                .progress='time')

# Format and plot
library(tidyr); library(ggplot2)
plot.dat <- gather(as.data.frame(result), sp, ab, node1:node5)

ggplot(plot.dat) + 
  geom_line(aes(time, ab, color=sp)) + 
  scale_x_sqrt()
