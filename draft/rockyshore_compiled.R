
# 

# Warning: order matters !!!
params <- list(K   = c(1,10),
               r   = c(.15  ,0),
               x   = c(0,.1),
               ats = c(0,0,
                       .11,0))

compile_load('./draft/rockyshore_compiled.c')

system.time(
  result <- lapply(as.list(seq.int(500)), 
                   function(x) {
                   ode(y=runif(2,.1,10), 
                       seq(0,120,l=100),
                       parms=unlist(params),
                       func='derivs', 
                       dllname='rockyshore_compiled', 
                       initfun="initmod",
                       nout=1)
                   })
)

result.fmt <- mapply(cbind, 
                     as.list(seq.int(500)), 
                     result, 
                     SIMPLIFY=FALSE) %>% 
                do.call(rbind,.) %>% 
                .[ ,1:4] %>%
                as.data.frame()

names(result.fmt) <- c('n','time','node1','node2')
result.fmt <- tidyr::gather(result.fmt, sp, ab, node1:node2)

library(ggplot2)
ggplot(result.fmt) + 
  geom_line(aes(time, ab, group=as.factor(n)), alpha=.1) + 
  facet_grid(~sp)

# par(mfrow=c(1,2))
# plot(result[ ,'time'], result[ ,'1'], type='l')
# plot(result[ ,'time'], result[ ,'2'], type='l')
