
# Define a node 

# Model parameters
r1 <- 1.1
r2 <- 1 + 1e-3

# Feeding matrix (per-capita rate)
M <- matrix(c(0, .1, 
              0, 0), byrow=TRUE, ncol=2)

# Define system
node1 <- logistic_growth(r1, 1) %+% eat_proportional_m(M)
node2 <- logistic_growth(r2, 1) %+% eat_proportional_m(M)
sysfun <- as.system(list(node1,node2))

# Solve 
solution <- ode(c(.001,1e-11), 1:1e3L, sysfun, parms=NULL)
colnames(solution) <- c('time', 
                        paste0("sp_",colnames(solution)[-1]))

# Format and plot
plot.dat <- tidyr::gather(as.data.frame(solution), sp, ab, sp_1:sp_2)

qplot(time, ab, color=sp, data=plot.dat, geom='line')
