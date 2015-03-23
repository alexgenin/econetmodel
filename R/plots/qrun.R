# 
# Quicly run/format/plot results of a system preset
# 

qplotsys <- function(dat) { 
  
  plot.obj <- ggplot(dat) + 
                geom_line(aes(time, ab, color=sp, group=id)) 
  
  return(plot.obj)
}

# Select relevant time ranges
qselect <- function(dat) { 
  
  in_range1 <- with(dat, time >= 0 & time <= 300)
  in_range2 <- with(dat, time >= 2900 & time <= 3400)
  
  # Range factor variable
  dat$ranges <- factor(rep("initial", nrow(dat)), 
                       levels=c("initial", "after_removal"))
  dat$ranges[in_range2] <- "after_removal"
  
  dat <- subset(dat, in_range1 | in_range2)
  dat
}

# Format data from a quick run
qformat <- function(dat,is.gathered=FALSE) { 
  if (!is.data.frame(dat)) { 
    dat <- as.data.frame(dat)
  }
  
  if ( ! is.gathered ) { 
    cols <- colnames(dat)[is_nodecol(colnames(dat), prefix="sp")]
    dat <- gather_(dat, "sp", "ab", cols) 
  }
  
  return(dat)
}

# Quickly runs 10 replicates of a system  
qrun <- function(preset, 
                 nruns=10,
                 ...) {  # passed to plyr
  
  # Load and compile system
  qrun_sys <- preset(...)
  system <- compile.system(qrun_sys)
  
  ddply(data.frame(id=seq.int(nruns)), ~ id, 
        function(id) { 
          system %>%
            alter_system(list(state=runif(get_size(system), 0, 1))) %>%
            run %>% 
            zero_below(1e-10) %>% 
            cbind(id, .) %>%
            adjust_names()
        })
}
