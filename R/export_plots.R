
plot_samplerun <- function(result, sys = attr(result, 'system')) { 
  
  if (!is.data.frame(result)) { 
    result <- as.data.frame(result)
  }
  
  result_fmt <- gather_(result, "sp", "ab", get_species(sys))
  plot <- ggplot(result_fmt) + 
            geom_line(aes(time, ab, group = id), alpha = .3) + 
            facet_grid( sp ~ range, scales = 'free')
  
  plot
}
