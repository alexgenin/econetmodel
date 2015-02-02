
# Note: we load libraries as quickly as possible
library(deSolve)
library(rootSolve)
# Helper function
N.nodes <- 4
as.sysmat <- function(vec) matrix(vec, ncol=N.nodes, byrow=TRUE)


## Resources growth
rs <- c(.3, .3, 0, 0) # rate
Ks <- rep(10, 4)   # K

## Trophic interactions 
# Per capita attack rate
fs <- as.sysmat(c(0,   0, -.2,  0,
                  0,   0, -.1,  -.01,
                  .2, .1,   0, -.1,
                  0,   .01,  .1,  0))
# Functional response parameter (halfsat level)
B0 <- matrix(10, ncol=N.nodes, nrow=N.nodes) 

## Mortality rates
ms <- c(0.02, 0.02, 0.01, 0.01) # mortality rate

# Define node 
sysfun <- function(time, X, parms) { 
            X[X<=0] <- 0
            dX <- loggrowth(X, rs, Ks) + 
                    rowSums(fs * frtype2(X, fs, B0)) + 
                    mortality(X, ms) 
            dX[X<=0] <- 0
            list(dX)
          }

## Initial state 
syslist <- syslist_create(sysfun, 0, rep(.1,N.nodes))

## OK, we run the system
result <-  syslist %>% run_to_eq() %>% remove_species(4) %>% run_to_eq()

# Format and plot
library(ggplot2)
library(tidyr)
plot.dat <- gather_(as.data.frame(result$state), "sp", "ab", 
                    paste0("node",seq.int(N.nodes)))
qplot(time, ab, color=sp, data=plot.dat, geom='line') 
