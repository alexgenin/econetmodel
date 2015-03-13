
library(deSolve)
library(magrittr)
library(ggplot2)
library(plyr)
library(tidyr)
library(rollply) # for grid-building functions

# 
# 4 producers
# 2 intermediate consumers
# 2 top predators
# 
# 
foodweb <- syspreset_rockyshore(tmax=3000, 
                                remove_species=FALSE) 
foodweb.c <- compile.system(foodweb) # side-effects !

test_coexistence <- function(rundat, fw) { 
  new.atk <- list(list(from=c(5,6), to=c(1,2,3,4), val=rundat[ ,'atk.gr']),
                  list(from=c(7,8), to=c(5,6),     val=rundat[ ,'atk.tp']))
  new.atk <- gen_interaction_matrix(new.atk, get_size(fw))
  
  fw %>%
    alter_system(list(state=runif(get_size(fw),.001, 1))) %>% 
    alter_parms(list(atk = new.atk)) %>%
    run  %>% 
    zero_below(1e-30) %>% 
    (function(mat) { mat[mat[ ,1] > 2900 & mat[ ,1] < 3000, ] }) %>%
#     select_range(2900,3000) %>%
    adjust_names 
}

system.time({
  rundat <- build_mesh_grid_identical(data.frame(atk.tp=c(0,2),
                                                 atk.gr=c(0,2)),
                                      1e3L)
  rundat <- cbind(id=seq.int(nrow(rundat)), rundat)
  
  result <- ddply(rundat, names(rundat), 
                  test_coexistence, foodweb.c, 
                  .progress='none')
})

result %>% 
  ddply(~ id, colMeans) %>%
  mutate(producers = (sp1==0) + (sp2==0) + (sp3==0) + (sp4==0),
         grazers   = (sp5==0) + (sp6==0),
         consumers = (sp7==0) + (sp8==0)) ->
  result.fmt 

ggplot(gather(result.fmt, key=sp, value=ab, sp1:sp8)) +
  geom_tile(aes(atk.gr, atk.tp, fill=ab)) + 
  facet_wrap(~ sp) 

gather(result.fmt,
       key=Trophic_level, value=Extinctions, 
       producers, grazers, consumers) %>% 
  ggplot() +
  geom_tile(aes(atk.gr, atk.tp, fill=as.factor(Extinctions))) + 
  facet_grid( ~ Trophic_level)

# Define what one run is supposed to be (fw is the foodweb object)
do_onerun <- function(rundat, fw) { 
  alter_system(fw, list(state=runif(Nsp,.001, 1))) %>% 
    run  %>% 
    zero_epsilon(., 1e-15) %>% 
    adjust_names %>% 
    select_ranges(., get_kept_output(fw)) %>% # becomes df at this step
    gather(., key="sp",value="ab", sp1:sp8)  # <!todo!> do not hardcode colnames
}

system.time({
  nruns <- 10000
  rundat <- data.frame(id     = seq.int(nruns), 
                       atk.gr = runif(nruns,0,2),
                       atk.tp = runif(nruns,0,2))
  result <- ddply(rundat, names(rundat), 
                  do_onerun, foodweb.c, 
                  .progress='time')
})


# Show species timeseries excerpt
ggplot(subset(result,range %in% c('b.before','c.removed'))) + 
  geom_line(aes(time, ab, csolor=sp, linetype=as.factor(id))) + 
  facet_grid(sp ~ range, scales='free_x') + 
  scale_y_sqrt()

# Analyse species abundances
spab <- ddply(result, 
              ~ id + range + sp + atk.gr + atk.tp, summarise,
              ab.median = median(ab), 
              .progress="time")

ggplot(spab) + 
  geom_line(aes(range, ab.median, group=paste(id)), alpha=.1) + 
  facet_grid(~ sp) + 
  scale_y_sqrt()

extinctions <- ddply(spab, ~ id + range + atk.gr + atk.tp, 
                     summarise, extinct = sum(ab.median == 0))
ggplot(extinctions) + 
  geom_line(aes(range, extinct, group=id), alpha=.2)

  
# ggplot(subset(result)) + 
#   geom_density(aes(ab.mean, color=range)) + 
#   facet_grid(sp ~ range, scales='free') 

# ggsave(plot=result.plot, 
#        filename="../output/plots/draft/test.png")

