# 
# Evaluates the performance of a system

performance <- function(syslist,pipeline) {
  
  # Run xx time steps
  pipeline <- function(x,tmax) run(x,times=tmax) 
  
  xtimes <- 2^seq.int(16)+1
  times <- sapply(xtimes,
             function(x) system.time(pipeline(syslist, x))
            )
  
  dat <- data.frame(sys.name=rep(as.character(substitute(syslist)), length(xtimes)),
                    tmax=xtimes, 
                    tcpu=times['elapsed', ])
  
  return(dat)
}
