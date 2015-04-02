# 
# 

# 
nt_topology <- matrix(runif(64,-1,1), ncol=8, nrow=8)
diag(nt_topology) <- 0

# Create two systems
sys_withnti <- syspreset_rockyshore_nti(tmax=5000) %>% 
                 set_removal(species=5, at=3000) %>%
                 alter_parms(nt=nt_topology) %>% 
                 compile.system()
sys_trophic <- sys_withnti %>% alter_parms(nt=0*nt)


# Show species by trophic level
layout <- data.frame(x=c(1,2,3,4, 2,3, 2,3),
                     y=c(0,0,0,0, 1,1, 2,2))

ggplot() + 
  geom_segment(aes(x, y, xend=xend, yend=yend), 
            data=links(sys_withnti, atk, layout), 
            alpha=.2,
            lineend='round') + 
  geom_arc(aes(x,y,xend=xend,yend=yend, 
               alpha=abs(value),
               color=as.factor(sign(value))), 
            data=links(sys_withnti, nt, layout, 0)) +
  geom_point(aes(x,y), size=5, data=layout) 
