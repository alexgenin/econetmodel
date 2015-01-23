
# Helper function
N.nodes <- 3
as.sysmat <- function(vec) matrix(vec, ncol=N.nodes, byrow=TRUE)

# Feeding matrix (per-capita rate)
fs <- as.sysmat(c(0, 0, -.2, 
                  0, 0, -.2, 
                  .3,.3,  0))
rs <- c(1.1, 1.04, 0)
ks <- rep(10,3)
ms <- c(0.02, 0.02, 0.01)

# Define functional responses
  
# ks <- function(id, X) {  }

# Define interaction functions
intrinsic_growth     <- function(id, X) X[id] * rs[id] * ( 1 - X[id]/ks[id] )
interactions         <- function(id, X) sum(X[id] * X * fs[id, ])
mortality            <- function(id, X) - X[id] * ms[id]
immigration          <- function(id, X) 0.0001

# Define system
sys.list <- list(
  algae_1 = intrinsic_growth %p% interactions %p% mortality %p% immigration,
  algae_2 = intrinsic_growth %p% interactions %p% mortality %p% immigration,
  grazer  =                      interactions %p% mortality %p% immigration
  )
sysfun <- as.system(sys.list, compile=TRUE)

# Solve 
library(deSolve)

solution <- system.time(ode(c(.001,.001, 1e-11), 
                            seq(1,10000,length.out=10000), 
                            sysfun, parms=NULL))

colnames(solution) <- c('time', 
                        paste0("sp_",colnames(solution)[-1]))

# Format and plot
plot.dat <- tidyr::gather(as.data.frame(solution), sp, ab, sp_1:sp_3)
ggplot2::qplot(time, ab, color=sp, data=plot.dat, geom='line') + 
  scale_y_sqrt() + 
  scale_x_sqrt()

