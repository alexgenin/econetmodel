# 
# 
# Plots the relevant information for a given system object
# 


system_plot <- function(sys, layout, ...) { 
  
  # Gather matrices to use for plotting 
  parms_to_plot <- match.call(expand.dots=FALSE)[['...']]
  parms_to_plot <- lapply(parms_to_plot, eval, 
                          envir=get_parms(sys), enclos=parent.frame())
  
  # Create base object
  plot_obj <- ggplot() + 
                geom_point(aes(x=x,y=y), size=4, data=layout)
  
  
  # Add each layer for the parameters to be plotted
  for (i in seq_along(parms_to_plot)) { 
    if ( i%%2 == 0 ) { # first goes to else -> segment
      plot_obj <- plot_obj + 
                    geom_arc(aes(x,y,xend=xend,yend=yend), 
                             data=as_edges(layout, parms_to_plot[[i]]))
    } else { 
      plot_obj <- plot_obj + 
                    geom_segment(aes(x,y,xend=xend,yend=yend), 
                                 data=as_edges(layout, parms_to_plot[[i]]), 
                                 color='red')
    }
  }
  
  return(plot_obj)
}