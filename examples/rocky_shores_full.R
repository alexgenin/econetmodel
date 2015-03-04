# 
# Full rockyshore model.
# 
# 
# 4 producers
# 2 intermediate consumers
# 2 top predators
# 
# 

Nsp <- 8
spmat <- make.squaremat(Nsp)

# Define some parameters
bodyms <- c(1,1,1,1, 
            10, 10,
            100, 100)

# Generate metabolic rates
mr0     <- 0.2227 # ?
mr0exp  <- -.25   # yes !
xs <- gen_met_rates(bodyms, mr0, mr0exp);

# Generate atk rates
a0  <- 27.23734
eps <- 0.01064

# Define topology: who eats whom ?
trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                         list(from=c(7,8), to=c(5,6)))
trophic_topology <- sysmat_fill(trophic_topology, Nsp)
atks             <- trophic_topology * 
                      gen_atk_rates(bodyms, xs, a0, eps) / 10

# Comsumption rates
ws <- t(apply(trophic_topology, 1, 
              function(X) if (sum(X)>0) X/sum(X) else X))

is_producer <- c(rep(TRUE,4), rep(FALSE,Nsp-4))

parameters <- list(
  # Producers' logistic growth
  # growth rate
  r  = 1 * is_producer,
  # carrying capacities (always > 0 !)
  K  = rep(1, Nsp),
  # Consumption
  # conversion efficiencies
  e  = spmat(0.85), 
  # consumption rates
  w  = ws, 
  # handling times
  h  = 1/(8*xs),
  # metabolic rates (plants have one)
  x  = xs,
  # attack rates (not as which is a reserved keyword in c++)
  atk = atks,
  # Removed species
  removed_species = 4
)

# Set time series parameters
time <- 0 
timestep <- 4
tmax <- 10e3L
foodweb <- 
  create.system(
    list(time          = time,
         timestep      = timestep,
         tmax          = tmax,
         state         = rep(max(parameters$K),Nsp),
         parms         = parameters,
         source        = list(template = './src/templates/rockyshore_full.c.template'),
         solver_parms  = list(func     = 'derivs',
                              initfunc = 'initmod', 
                              events   = list(func = 'remove_species',
                                              root = FALSE,
                                              time = nearestEvent(2500, seq(0,tmax,by=timestep)),
                                              terminalroot=2), # the root max nb of eq 
                              nout     = 2,
                              outnames = c("maxd","remaining_eq"),
                              rootfunc = 'controlf',
                              nroot    = 2,
                              verbose  = FALSE)
         ))

foodweb.c <- compile.system(foodweb) # side-effects !

system.time(
  (run(foodweb.c))
)

nruns <- 500
rundat <- data.frame(id=seq.int(nruns), 
                     atk_top_int=seq(0,.5, length.out=nruns))

do_onerun <- function(rundat) { 

  # Create new matrix 
  new.atk             <- get_parms(foodweb.c)[['atk']]
  new.atk[c(7,8),5:6] <- rundat[1,'atk_top_int'] 
  new.atk             <- new.atk * trophic_topology
  
  alter_parms(foodweb.c, 
              list(atk = new.atk)) %>%
    alter_system(., list(state=runif(Nsp,.001, 1))) %>% 
    run %>% 
    export(., gather.do=TRUE) 
}


system.time(
  result <- ddply(rundat, names(rundat), do_onerun, .progress='time')
)


library(ggplot2)

ggplot(result) + 
  geom_line(aes(time, ab, color=atk_top_int, group=id), 
            alpha = .2, size = .7) + 
  facet_grid(sp ~ .)
