
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

system.time(
  test <- mrun(sys, 12*120,
               . %>% 
                 # Set initial parameters
                 alter_system(state = runif(get_size(sys), 0, 1)) %>% 
                 set_removal(species = oneof(removal_cases), at = 3000) %>% 
                 alter_parms(q = runif(1,0,1)) %>% 
                 
                 # Add competition between mussels (5) and algae (1 to 4)
                 alter_parms(c = {c[5, c(1,2,3,4)] <- runif(1,atk[5,1],
                                                              atk[5,1]); 
                                  c }) %>% 
                 
                 # Add whelks avoidance by grazers 
#                  alter_parms(datk = { datk <- array(0, rep(get_size(sys),3))
#                                       datk[6, 7, 6] <- atk[7,6] / 10
#                                       datk }) %>% 
                 
                 # Add facilitation of algae by mussels
#                  alter_parms(dK = { dK <- atk*0 # N*N matrix filled with zeros
#                                     dK[5, c(1,2,3,4)] <- max(K) * 1.2;
#                                     dK }) %>% 
                 
                 # Run simulation 
                 run() %>% 
                 zero_below(1e-5) %>% 
                 discard_if_extinct(before = 3000, 1e-5) %>% 
                 adjust_names() %>% 
                 
                 # Select and format results
                 format_samplerun() %>% # format for a sample run output
#                  format_eq() %>% # format for an abundance@eq graph
                 insert_removal_case_rs(prefix = "rm") %>% 
                 insert_parms(algae_compet = max(c), 
                              q = q) %>% # q in functional response 
                 mutate(algaem = algae1+algae2+algae3+algae4),
                 
               # Computing options
               .progress='time',
               .parallel=parjob())
)

# 36k replicates ~= 25 minutes w/ 12 cores ( ~ 2/s)
# 29k replicates ~= 33 minutes w/ 12 cores 

# Run only if format_eq() in chain
ggplot( gather(test, sp, ab, grazers, mussels) ) + 
  geom_boxplot(aes(x = sp, y = ab, fill = sp)) + 
  facet_grid( rm2 ~ rm1, scales='free_y') + 
  ylab('biomass') + 
  xlab('Second species removed') 


# Run only if format_samplerun() in chain
# plot_samplerun(test)
ggplot( subset(gather(test, sp, ab, 
                      algaem, grazers, mussels, whelks, crabs, phyto),
               rm1 == 'crabs' & rm2 == '-') ) + 
  geom_line(aes(time, ab, group = paste(sp,id), color = id), alpha = .2) + 
  facet_grid(sp ~ range, scales = 'free')

