
# Note: we load libraries as quickly as possible
library(deSolve)
library(rootSolve)
library(magrittr)

# Helper function
as.sysmat <- function(vec) matrix(vec, ncol=4, byrow=TRUE)

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
B0 <- matrix(10, ncol=4, nrow=4) 

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
syslist <- syslist_create(sysfun, 
                          init.time=0, 
                          X=rep(.1,4),
                          state.names.skl='node')

## OK, we run the system
result <-  syslist %>% 
             run_to_eq() %>% 
             remove_species(4) %>% 
             run_to_eq()

## Do the test
mapply(expect_equal, 
       last_state(result), 
       c(node1=6.529276,node2=8.487123,node3=17.726401,node4=0.000000),
       MoreArgs=list(tolerance=1e-7))

