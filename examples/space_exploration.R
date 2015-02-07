# 
# Explore a parameter space
# 
library(plyr)
library(magrittr)

# Helpers
N.nodes <- 4
as.sysmat <- function(vec) matrix(vec, ncol=N.nodes, byrow=TRUE)

## Define parameters
parms <- list(rs = c(.3, .3, 0, 0), # rate
              Ks = rep(10, 4),   # K
              B0 = matrix(10, ncol=N.nodes, nrow=N.nodes),
              ms = c(0.02, 0.02, 0.01, 0.01), # mortality rate
              fs = as.sysmat(c(0,   0, -.2,   0,
                               0,   0, -.1,-.01,
                               .2, .1,   0, -.1,
                               0, .01,  .1,   0)) )

## Define system
sysfun <- function(time, X, parms) { 
            X[X<=0] <- 0
            dX <- with(parms,
                       loggrowth(X, rs, Ks) + 
                       rowSums(fs * frtype2(X, fs, B0)) + 
                       mortality(X, ms))
            dX[X<=0] <- 0
            list(dX)
          }

## Define analysis pipeline
pipeline <- function(x) { 
#   parms[['fs']][4,3] <- x$a34
#   parms[['fs']][3,4] <- -x$a34
  
  syslist_create(sysfun, 
                  init.time=0, 
                  X=runif(4,0,20), 
                  tres=.2,
                  parms=parms) %>% 
  run_to_eq(full.calc=TRUE) %>%
  .[['state']]
}

## Compute
jobs <- data.frame(a34=seq(0,1,length=100))
result <- ddply(jobs, names(jobs), 
                pipeline, 
                .progress='time')

## Format and plot
library(ggplot2)
# library(tidyr)
# plot.dat <- gather_(as.data.frame(result), "sp", "ab", 
#                     paste0("node",seq.int(N.nodes)))
ggplot(result) + 
  geom_path(aes(time,node4, group=a34), alpha='.2') 
#   scale_x_sqrt()
  

