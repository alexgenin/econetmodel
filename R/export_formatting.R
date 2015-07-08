
# Format output of a result object to display sample runs 
format_samplerun <- function(result, 
                             sys = attr(result, 'system')) { 
  
  result %>% 
    select_ranges(at_removal = c(removal_time - 50, removal_time + 200), 
                  final      = c(tmax - 50, tmax), 
                  add.factor = TRUE) 
  
}


# Format
format_eq <- function(result, 
                      sys = attr(result, 'system')) { 
  
  result %>% 
    select_ranges(final = c(tmax - 200, tmax), summarise.fun = mean)
  
}
