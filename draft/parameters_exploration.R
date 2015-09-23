library(devtools)
library(plyr)
document()

# Define the system
sys <- syspreset_rockyshore(tmax=10e3, timestep=4) %>%
  compile(quiet=FALSE) %>% 
  alter_system(solver_parms=list(method='ode45')) # lsodar gives errors w/ nt>0

removal_cases <- list(p1   = "whelks", 
                      p2   = "crabs",
                      gp1  = c("mussels", "whelks"), 
                      gp2  = c("mussels", "crabs"), 
                      gp3  = c("grazers", "whelks"), 
                      gp4  = c("grazers", "crabs"),
                      g1   =  "grazers",
                      g2   =  "mussels",
                      none = "-")

# We define some common operations to all simulations
set_init_parms <- . %>% 
  alter_system(state = runif(get_size(sys), 0, 1)) %>% 
  set_removal(species = oneof(removal_cases), at = 3000) %>% 
  alter_parms(q = runif(1,0,1)) 

format_results_base <- . %>% 
  zero_below(1e-5) %>% 
  discard_if_extinct(before = 3000, 1e-5) %>% 
  adjust_names() 
  
add_parms_to_result <- . %>% 
  insert_removal_case_rs(prefix = "rm") %>% 
  insert_parms(algae_compet = max(c), 
               algae_dK = min(dK),
               tp_mussels = mean(datk[datk!=0]),
               q = q) # q in functional response 

NREPLICATES <- 11*40

# Intensity of non-trophic processes
# Keep this below 3/4 or algea die
compet_max   <- with(get_parms(sys), atk[6,1] / 3  )


for (i in seq.int(4)) { 
  randomize_parms <- function(sys) { 
    parms <- get_parms(sys)
    
    # Randomize attack rates (but not topology)
    parms[['atk']] <- with(parms, (atk>0) * runif(length(atk), 0, 20))
    # Randomize variation of attack rates 
  #   parms[['datk']] <- { 
  #     datk <- array(0, rep(get_size(sys),3))
  #     }
    
    sys$parms <- parms
    sys
  }

  # Run model
  test <- mrun(sys, NREPLICATES,
                . %>% 
                  # Set initial parameters
                  set_init_parms() %>% 
                  randomize_parms() %>% 
                  # Add competition
  #                 alter_parms(c = {c[5, c(1,2,3,4)] <- runif(1, 0, compet_max); c}) %>% 
                  alter_parms(dK = { dK <- (atk*0);
                                    dK[5, c(1,2,3,4)] <- - runif(1, .5, 1) 
                                    dK},
                              yt = yt/yt * .1) %>% 
                  # Run simulation
                  netmodr::run() %>% 
                  # Select and format results
                  format_results_base() %>%
                  select_ranges(base = c(tmax - 200, tmax), 
                                rm   = c(2800, 2999),
                                summarise.fun = mean) %>% 
  #                 chbrowser() %>% 
                  add_parms_to_result(),
                # Computing options
              simplify = FALSE,
              .progress = 'time',
              .parallel = parjob())

  check_result <- function(df) {
    # Codes expected results (1 if increases, 0 if decreases)
    expectation <- 
      rbind(data.frame(rm1 = 'crabs',  rm2 = '-',       mussels = 1, grazers = 0),
            data.frame(rm1 = 'whelks', rm2 = '-',       mussels = 1, grazers = 0),
            data.frame(rm1 = 'crabs',  rm2 = "mussels", mussels =-1, grazers = 1),
            data.frame(rm1 = 'crabs',  rm2 = "grazers", mussels =-1, grazers = -1),
            data.frame(rm1 = '-',      rm2 = 'mussels', mussels =-1, grazers = 0),
            data.frame(rm1 = '-',      rm2 = 'grazers', mussels =-1, grazers = -1))
    
    current_case <- subset(expectation, rm1 == df[ ,'rm1'] & rm2 == df[ ,'rm2'])
    
    cond1 <- (current_case[ ,'mussels'] == 0) || 
              sign(diff(df[order(df[ ,'time']),'mussels'])) == current_case[ ,'mussels']
    cond2 <- (current_case[ ,'grazers'] == 0) || 
              sign(diff(df[order(df[ ,'time']),'grazers'])) == current_case[ ,'grazers']
    
    return( cond1 & cond2 )
  }
  
  test_correct <- Filter(check_result, test)

  lapply(test_correct, . %>% attr('system') %>% get_parms())
  
}
