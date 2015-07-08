
## 
## System definition
## ----------------------------------------------

# Set initial parameters
set_init_parms <- . %>% 
  alter_system(state = runif(get_size(sys),0,1)) %>% 
  set_removal(species=oneof(list(m=5, 
                                 m2=6))) %>% 
  alter_parms(q = runif(1,0,1))

# Define the system
sys <- syspreset_rockyshore(tmax=10e3, timestep=1) %>%
  compile(quiet=FALSE) %>% 
  alter_system(solver_parms=list(method='ode45')) # lsodar gives errors w/ nt>0

NSIMUS <- 120
.USEPARALLEL=FALSE

## 
## NTI scenarios
## ----------------------------------------------

add_pred_avoidance_tt <- . %>% 
  # We draw the effect of species 8 on 6's attack rates between 0 and -atk[6,1]
  #   (which is equals to any nonzero -atk[6,*]
  alter_parms(datk={ datk <- array(0, c(8,8,8))
                     howmuch <- runif(1, 0, atk[7,5]/15)
                     datk[8, 7, c(5,6)] <- - howmuch
                     datk[7, 8, c(5,6)] <- - howmuch
                     return(datk) }) 

## 
## Result formatting
## ----------------------------------------------

# Format 
insert_some_parms <- . %>% 
  insert_parms(q=q) %>% 
  insert_parms(dK=ifelse(any(dK!=0), mean(dK[dK!=0]), 0)) %>% 
  insert_parms(datk.mean = ifelse(any(datk!=0), mean(datk[datk!=0]), 0), 
               datk.max  = ifelse(any(datk!=0), max(datk[datk!=0]),  0)) %>% 
  insert_removal_case(as='rmcase') 

format_samplerun <- . %>% 
  adjust_names("sp") %>% 
  select_ranges(before = c(removal_time - 200*timestep, 
                           removal_time + 200*timestep),
                after  = c(tmax - 100*timestep, tmax),
                add.factor = TRUE) %>% 
  insert_some_parms()
  
format_bifdiag <- . %>% 
  adjust_names("sp") %>% 
  select_ranges(before = c(removal_time - timestep),
                after  = c(tmax),
                add.factor = TRUE) %>% 
  biomass_by_tlvl(trophlvls=c('b','b','b','b','m','m','t','t')) %>% 
  discard_if_extinct(before=3000, 1e-5) %>%
  insert_some_parms()

format_rm_analysis <- . %>% 
  adjust_names('sp') %>% 
  select_ranges(before = c(removal_time - 200*timestep, 
                           removal_time),
                after  = c(tmax - 200*timestep, tmax),
                add.factor = TRUE) %>% 
  discard_if_extinct(before=3000, 1e-5) %>%
  insert_removal_case(as='rmcase') %>% 
  compute_stats('before', 'after', 
                list(stats_effect_sizes, stats_sec_extinctions, 
                     stats_effect_biomass, stats_effect_sizes_bytroph),
                species_cols=sp1:sp8, removal_col="rmcase",
                trophlvls=c('b','b','b','b','m','m','t','t')) %>%
  insert_some_parms()

## 
## Pred avoidance (t <-> t)
## ------------------------------------------ 

# Define a simulation
simu_predav <- . %>% 
  set_init_parms() %>% 
  add_pred_avoidance_tt() %>% 
  run() %>% 
  zero_below(1e-5)

result_predav <- mrun(sys, NSIMUS, 
                      . %>% simu_predav %>% format_samplerun(), 
                      .progress='time', .parallel=parjob()) %>% 
                      ( function(dat) { dat$datk.range <- cut(dat$datk.mean,5);dat } )
