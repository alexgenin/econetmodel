# 
# 
# This file contains code that will create a file containing data the simulation
#  runs.
# 

# Package dependencies
library(ggplot2)
library(devtools)


# Set working dir
wd <- '/home/alex/work/2015-2016/network_modelling/output/paper/ian_paper/figures/'
setwd(wd)

# Set package dir and load it 
packdir <- "/home/alex/work/2015-2016/network_modelling/dev/"
document(packdir)

# Figure dir = current dir
figdir <- getwd()

# Strength of trophic interactions
a0 <- 0.05

# Load basic packages
library(devtools)
library(plyr)
library(rmarkdown)
library(magrittr)
library(deSolve)
library(tidyr)

# We set knitr options and load package functions
knitr::opts_chunk$set(warning = FALSE, cache = TRUE, echo = FALSE, 
                      fig.width = 10, fig.height = 10*(6/8))

# Load network modelling package
path_to_git_repo <- "/home/alex/work/2014-2015/network_modelling/dev/"
# We need to set the working dir to the package folder so files are found
# setwd(path_to_git_repo)
document(path_to_git_repo)

# Define the system
sys <- syspreset_rockyshore(tmax = 10e3, timestep = 1,
                            template = paste0(path_to_git_repo, 
                                              'src/templates/rockyshore_dynw.c.template'),
                            a0 = a0) %>% # a0 == 0.05
  compile(quiet = TRUE,
          lib.dir = '.', 
          include.dir = paste0(path_to_git_repo, 'src/include')) %>% 
  alter_system(solver_parms = list(method = 'ode45')) # lsodar gives errors w/ nt>0
# Bug: the system has to be named sys otherwise parallel processing does not 
# work ?! => Global in chainable functions ?

# Okay, the idea here is to have a `system` object that contains all the 
# information we need to actually run the simulation. All the functions
# add_compet/predav/etc modify this `system` object and return it. This way 
# we create a chain of functions that explicitely displays the modification 
# we do to our original system.
# 
# We can retrieve information from the system object by using get_parms,
# get_size, get_species, get_solver_parms, etc. or by directly accessing 
# its components: sys$parms




# This function modifies the system by adding competition from species 
#   5 (mussels) to species 1 (algae_in_compet). Get a list of species 
#   and their order by using get_species(sys)
add_compet <- function(sys, strength = .3) { 
                  alter_parms(sys, c = {
                      c[5, 1] <- strength
                      c # return c
                    })
              }

# This function sets the foraging preference of predators by adjusting a 
#   strength parameters. 
# 
# The base preference of predators for mussels is set to `.5 - strength`, 
#   and the rational function maximum is set to `.5 + 2 * strength`. The 
#   preference for grazers is adjusted accordingly so that the foraging 
#   preferences always sum to one.
# 
# For example: 
#   - if strength = 0 then no modfication is done to the foraging preferences
#       (w = .5, \delta w = 0)
#   - if strength > 0 then w_mussels = .5 - strength and dw = 2 * strength. 
#       For example if strength = 0.3, then base preference for mussels is set 
#       to .2 ( = .5 - .3), so predators tend to eat more grazers when alone. 
#       dw is also set to 0.6 (2 * 0.3) so that the preference of predator 
#       for mussels when the other is very abundant is close to 0.2 + 0.6 = 0.8.
# 
# NB : having a strength of .5 might produce negative foraging preference and 
#   hang the numerical integration (due to floating point errors).
add_foragpref <- function(sys, strength = .4) { 
                  
                  alter_parms(sys, 
                    w = { 
                      # Predators prefer grazers when alone. Note: for 
                      #   a predator, w_ij is set by default to 1/Npreys = .5
                      #   (no preference for both)
                      w[c(7,8), 5] <- w[c(7,8), 5] - strength # negative ! 
                      w[c(7,8), 6] <- w[c(7,8), 6] + strength
                      w
                    },
                    dw = { 
                      # Predators prefer mussels when both are present
                      dw <- maked(w) # create empty 3D array
                      dw[7, 8, 5] <- dw[8, 7, 5] <- + 2 * strength  # 5 = mussels
                      dw[7, 8, 6] <- dw[8, 7, 6] <- - 2 * strength  # 6 = grazers
                      dw 
                    })
              }


# This chain of functions accept a data frame as produced by the run() and 
# prepares the results of a simulation. 
prepare_result <- . %>% 
                    zero_below(1e-5) %>% # Zero abundances below this threshold
                    adjust_names() %>%   # Adjust names of result data frame
                    change_names(algae1 = 'algae_in_compet', # change some names
                                 algae4 = 'algae') %>% 
                    select_ranges(at_removal = c(4900, 5100), # select subset of 
                                  add.factor = TRUE) %>%      #   time series
                    insert_removal_case_rs(prefix = 'rm') %>% # add removal case
                    insert_parms(c = max(c),  # Add parameter values to the 
                                 q = q,       #   final data frame
                                 pavoid = dw[7, 8, 5])



# Here we define our 4 sets of simulations. Note that the species removal 
# is randomly taken out of a set because it is simpler to implement and 
# statistically we always end up with all removal cases. 

# Define a simulation with no NTIs
simu_none <- 
  . %>% 
    alter_system(state = c(rep(.5, 8))) %>% 
    alter_parms(q = .5) %>% 
    set_removal(oneof(list(c('grazers', 'whelks'),
                           c('mussels', 'whelks'),
                             'whelks',
                             'grazers')), at = 5000) %>% 
    netmodr::run() %>% # avoid namespace clash with rmarkdown::run
    prepare_result() 


# Define a simulation with only competition
simu_compet_only <- 
  . %>% 
    alter_system(state = c(rep(.5, 8))) %>% 
    alter_parms(q = .5) %>% 
    add_compet() %>% 
    set_removal(oneof(list(c('grazers', 'whelks'),
                           c('mussels', 'whelks'),
                             'whelks',
                             'grazers')), at = 5000) %>% 
    netmodr::run() %>% # avoid namespace clash with rmarkdown::run
    prepare_result() 
    
# Define a simulation with only foraging preference ("predatory avoidance")
simu_foragpref_only <- 
  . %>% 
    alter_system(state = c(rep(.5, 8))) %>% 
    alter_parms(q = .5) %>% 
    add_foragpref() %>% 
    set_removal(oneof(list(c('grazers', 'whelks'),
                           c('mussels', 'whelks'),
                             'whelks',
                             'grazers')), at = 5000) %>% 
    netmodr::run() %>% # avoid namespace clash with rmarkdown::run
    prepare_result() 
    
# Define a simulation with only foraging preference ("predatory avoidance")
simu_compet_foragpref <- 
  . %>% 
    alter_system(state = c(rep(.5, 8))) %>% 
    alter_parms(q = .5) %>% 
    add_compet() %>% 
    add_foragpref() %>% 
    set_removal(oneof(list(c('grazers', 'whelks'),
                           c('mussels', 'whelks'),
                             'whelks',
                             'grazers')), at = 5000) %>% 
    netmodr::run() %>% # avoid namespace clash with rmarkdown::run
    prepare_result() 


# Actually run all simulations
all_simulations <- list(none = simu_none, 
                        competition = simu_compet_only, 
                        predav = simu_foragpref_only, 
                        both = simu_compet_foragpref)

result <- ldply(all_simulations,
                function(simu, sys) { 
                  mrun(sys, 20, simu, .progress = 'none', .parallel = FALSE)
                }, 
                sys, .id = "simu", .progress = 'time', .parallel = FALSE)





# Results formatting happen below
# ----------------------------------------


# Take two lines out of the result data frame: one at equilibrium 
# before the removal and one at equilibrium after the removal.
result_subs <- subset(result, time == min(time) | time == max(time))

# Compute the relative increase/decrease of each species after removal
result_delta <- ddply(result_subs, ~ simu + id, 
                      function(dat) { 
                        for (col in c('algae_in_compet', 'algae', 
                                      'mussels', 'grazers', 
                                      'whelks', 'crabs')) { 
                          dat[ ,col] <- 
                            with(dat, 
                                 ( dat[ ,col] - dat[time == min(time), col] ) /
                                       dat[time == min(time), col])
                        }
                        dat
                      })

# Test whether there are extinctions happening after removal
result_extinctions <- ddply(result_subs, ~ simu + id, 
                            function(dat) { 
                              has_extinctions <- with(dat, 
                                # 0 algae after removal
                                dat[time == max(time), 'algae_in_compet'] == 0 &
                                # but >0 algae before removal
                                dat[time == min(time), 'algae_in_compet'] > 0)
                              
                              data.frame(rm1 = dat[1, "rm1"],
                                         rm2 = dat[1, "rm2"], 
                                         extinctions = has_extinctions)
                            })

# Format output to have nice labels in plots
result_delta$simu_fmt <- result_delta$simu
levels(result_delta$simu_fmt) <- c('None', 'Comp. for space', 
                                   'Predatory avoidance', 
                                   'Comp. for space + \n Predatory avoidance')

result_extinctions$simu_fmt <- result_extinctions$simu
levels(result_extinctions$simu_fmt) <- c('None', 'Comp. for space', 
                                   'Predatory avoidance', 
                                   'Comp. for space + \n Predatory avoidance')

save.image('simu_data.rda')
