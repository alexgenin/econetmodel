# 
# Quicly run and plot results
# 

qplotsys <- function(dat, is.gathered=FALSE) { 
  
  if (! is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  
  if ( ! is.gathered ) { 
    cols <- colnames(dat)[is_nodecol(colnames(dat), prefix="sp")]
    dat <- gather_(dat, "sp", "ab", cols) 
  }
  
  plot.obj <- ggplot(dat) + 
                geom_line(aes(time, ab, color=sp, group=id)) + 
                facet_grid(sp ~ .)
  print(plot.obj)
}

# Quickly runs a system
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
            cbind(id, .) %>%
            adjust_names()
        })
}
