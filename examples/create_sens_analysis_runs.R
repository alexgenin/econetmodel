# 
# 
# This file contains code that will produce simulation runs required for the 
# sensitivity analysis
# 

# setwd(path_to_git_repo)

# Package dependencies
library(ggplot2)
library(devtools)
library(plyr)
library(magrittr)


# Set working dir
wd <- "/home/alex/work/2014-2015/network_modelling/dev/examples/"
setwd(wd)

# Set package dir and load it 
packdir <- "/home/alex/work/2015-2016/network_modelling/dev/"
document(packdir)


# We load previous results for simulations parameters et al
if ( file.exists('simu_data.rda') ) { 
  load('simu_data.rda', verbose = TRUE)
} else { 
  stop('The single run example needs to be run first (create_simu_runs.R)')
}

# 
# Define the system
sys <- syspreset_rockyshore(tmax = 10e3, timestep = 1,
                            template = paste0(path_to_git_repo, 
                                              'src/templates/rockyshore_dynw.c.template'),
                            a0 = a0) %>% # a0 == 0.05
  compile(quiet = TRUE,
          lib.dir = '.', 
          include.dir = paste0(path_to_git_repo, 'src/include')) %>% 
  alter_system(solver_parms = list(method = 'ode45')) # lsodar gives errors w/ nt>0

# Define our main simulation 
simu <-   . %>% 
  alter_system(state = c(rep(.5, 8))) %>% 
  alter_parms(q = .5) %>% 
  netmodr::run() %>% # avoid namespace clash with rmarkdown::run
  zero_below(1e-5) %>% 
  adjust_names() %>% 
  change_names(algae1 = 'algae_in_compet', 
               algae4 = 'algae') %>% 
  select_ranges(at_removal = c(4900, 5100),
                add.factor = TRUE) %>% 
  insert_removal_case_rs(prefix = 'rm') 

# Helper function to easily modify all of the parameters of interest
#   with given values. 
with_parms_set <- function(sys, removal, foragpref_strength, 
                           compet, atk.coef) { 
  sys %>% 
    add_foragpref(foragpref_strength) %>% 
    add_compet(compet) %>%  
    alter_parms(atk = atk * atk.coef) %>% 
    set_removal(removal, at = 5000) 
}

do_simu_and_check_result <- function(sys, foragpref_strength, 
                                     compet, atk.coef) { 
  
  # Do simulation for whelk removal 
  simu_w <- mrun(with_parms_set(sys, 
                                removal = 'whelks', 
                                foragpref_strength = foragpref_strength, 
                                compet = compet, 
                                atk.coef = atk.coef), 
                 1, simu)
  
  # Do simulation for whelk + grazers removal 
  simu_wg <- mrun(with_parms_set(sys, 
                                 removal = c('whelks', 'grazers'), 
                                 foragpref_strength = foragpref_strength, 
                                 compet = compet, 
                                 atk.coef = atk.coef), 
                 1, simu)
  
  # Do simulation for whelk + mussels removal 
  simu_wm <- mrun(with_parms_set(sys, 
                                 removal = c('whelks', 'mussels'), 
                                 foragpref_strength = foragpref_strength, 
                                 compet = compet, 
                                 atk.coef = atk.coef), 
                 1, simu)
  
  # Build output 
  
  # Test for instability (extinctions before removal)
  species <- c("algae", "algae_in_compet", "grazers", 
               "mussels", "whelks", "crabs")
  # All these should be equal
  unstable_w  <- any(simu_w[simu_w$time < 4999,   "algae_in_compet"] == 0)
  unstable_wg <- any(simu_wg[simu_wg$time < 4999, "algae_in_compet"] == 0)
  unstable_wm <- any(simu_wg[simu_wg$time < 4999, "algae_in_compet"] == 0)
  
  # Test for extinctions ONLY when whelks are removed
  has_extinctions_w <- 
    ifelse(unstable_w, NA, 
           any(simu_w[simu_w$time > 5000, 'algae_in_compet'] == 0))
  has_extinctions_wg <- 
    ifelse(unstable_wg, NA, 
           any(simu_wg[simu_wg$time > 5000, 'algae_in_compet'] == 0))
  has_extinctions_wm <- 
    ifelse(unstable_wm, NA, 
           any(simu_wm[simu_wm$time > 5000, 'algae_in_compet'] == 0))
  
  # Difference in mussels/grazers abundance
  
  # Result 1: 
  # We observe extinctions when only whelks are removed, but not when whelks 
  # are removed with a primary consumer (mussels or grazers)
  extinctions_ok <- ! has_extinctions_wg & ! has_extinctions_wm & 
                        has_extinctions_w 
  
  # Result 2: 
  # We consider the difference in mussels abundance ok when the increase is 
  # of a factor above 1.27 and below 1.6 (taken from figure 3 in the 
  # manuscript)
  # # 66+/-6 % -> 95+/-3 ==> 92/72 = 1.27 to 98/60 =  1.6
  diff_mussels <- with(simu_w, mussels[time == max(time)] / 
                                 mussels[time == min(time)]) # min(time) = 4900
  diff_mussels_ok <- diff_mussels > 1.2 & diff_mussels < 1.6 
  
  # Result 3: No increase in grazers when removing whelks
  # 
  diff_grazers_w <- with(simu_w, grazers[time == max(time)] / 
                               grazers[time == min(time)]) # min(time) = 4900
  diff_grazers_w_ok <- diff_grazers_w <= 1
  
  # Result 4: Increase in grazers when removing whelks
  # 
  # We consider the difference in grazers compatible with results when they 
  # increase of a factor of minimum 1.4 after removal of a predator and mussels
  # and below 3.2.
  # 18 +/- 3.5 39 +/- 8 => ratio of 3.2 to 1.44
  diff_grazers_wm <- with(simu_wm, grazers[time == max(time)] / 
                                 grazers[time == min(time)]) # min(time) = 4900
  diff_grazers_wm_ok <- diff_grazers_wm > 1.4 & diff_grazers_wm < 3.2 
  
  # Result_status: a variable that textually reminds the results we obtain 
  status <- c(ifelse(extinctions_ok,     '1', NA), 
              ifelse(diff_mussels_ok,    '2', NA), 
              ifelse(diff_grazers_w_ok,  '3', NA), 
              ifelse(diff_grazers_wm_ok, '4', NA))
  status <- paste0("Result ",paste(na.omit(status), collapse = ' + '))
  status <- ifelse(grepl("^Result $", status), "None", status)
  status[unstable_w] <- 'Unstable network'
  
  # Number of results reproduced
  status_number <- extinctions_ok + diff_mussels_ok + diff_grazers_w_ok 
                     + diff_grazers_wm_ok
  
  data.frame(foragpref_strength = foragpref_strength, 
             compet = compet, 
             atk.coef = atk.coef, 
             a0 = a0 * atk.coef,  # update a0 value
             mussels_before = with(simu_w, mussels[time == min(time)]),
             mussels_after =  with(simu_w, mussels[time == max(time)]),
             grazers_before = with(simu_w, grazers[time == min(time)]),
             grazers_after =  with(simu_w, grazers[time == max(time)]),
             diff_mussels = diff_mussels, 
             diff_mussels_ok = diff_mussels_ok, 
             diff_grazers_wm = diff_grazers_wm, 
             diff_grazers_wm_ok = diff_grazers_wm_ok, 
             diff_grazers_w = diff_grazers_w,
             diff_grazers_w_ok = diff_grazers_w_ok,
             unstable = unstable_w, # equal to all others
             has_extinctions_w = has_extinctions_w, 
             has_extinctions_wg = has_extinctions_wg, 
             has_extinctions_wm = has_extinctions_wm, 
             extinctions_ok = extinctions_ok, 
             status = status, 
             status_number = status_number,
             # Turn off converstion of strings to factors
             stringsAsFactors = FALSE)
}

# Make simu plan 
resolution <- 200
simu_plan <- list(foragpref_strength = seq(0, .49, length.out = resolution),
                  compet =   seq(0, .6, length.out = resolution),
                  atk.coef = c(.5, 1, 2)) # we use 100 for debug c(1, 10, 100))

simu_plan <- expand.grid(simu_plan)
simu_plan <- data.frame(id = seq.int(nrow(simu_plan)), simu_plan)

redo <- TRUE
if (redo) { 
  sens_results <- ddply(simu_plan, ~ id, 
                        function(dat) { 
#                           system('echo -n "." >> /home/alex/system/tmp/progress.tmp')
                          do_simu_and_check_result(sys, 
                                                   dat[ ,'foragpref_strength'], 
                                                   dat[ ,'compet'], 
                                                   dat[ ,'atk.coef'])
                          }, .progress = 'time')
  
  setwd(wd) # Get back to figure folder
  save.image("sensitivity_data.rda")
  
} else { 
  load("sensitivity_data.rda")
}

