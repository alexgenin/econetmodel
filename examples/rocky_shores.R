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
    r  = c(1, rep(0,size-1)), 
    # carrying capacities
    K  = rep(10, size),
  # Consumption
    # conversion efficiencies
    e  = sysmat(0.85), 
    # consumption rates
    w  = ws, 
    # handling times
    h  = 1/(8*xs),                   
    # metabolic rates (plants have one)
    x  = xs,
    # attack rates (not as as it is a reserved keyword in cpp and we want to be consistent)
    atk = atks
)

clean_c_code("./src/templates/")
gen_c_code(parameters, "./src/templates/rockyshore.c.template", overwrite=TRUE)
document()
dyn.load('./src/netmodr.so')

replicate_groups <- as.list(seq.int(500))
system.time(
  result <- lapply(replicate_groups, function(x) { 
              ode(y=runif(Nsp,.1,10), 
                times=seq(0,120,l=1000),
                parms=unlist(parameters),
                func='rockyshore', 
                initfun="initmod",
                dllname='netmodr', 
                nout=1)
            })
)

for (i in seq.int(length(result))) { 
  result[[i]] <- result[[i]][ ,-ncol(result[[i]])]
  colnames(result[[i]]) <- c("time", paste0("node",seq.int(Nsp)))
  result[[i]] <- cbind(N=i,result[[i]])
}

result <- do.call(rbind, result)

# Format and plot
library(tidyr)
library(ggplot2)

plot.dat <- gather(as.data.frame(result), sp, ab, node1:node5)

ggplot(subset(plot.dat)) + 
  geom_line(aes(time, ab, group=N), alpha=.1) + 
#   geom_point(aes(time, ab, color=sp)) + 
  facet_grid(sp~.) +
  scale_x_sqrt() 
#   ylim(c(0,3))

