# 
# Functions that take a system object and return understandable stuff
# 

# zero_epsilon <- function(result, zerotol) { 
#   
#   result[result<zerotol] <- 0;
#   return(result)
# }

compute_stats <- function(df, time_ranges, 
                          what=list(min,max,mean,median)) { 
  
  time_ranges %>%
    lapply(is_in_range) %>%
    lapply(function(x) { subset(result[ ,x]) }) 
    
  
}

# Select time range(s)
select_ranges <- function(result, time_ranges) { 
  time_ranges %>%
    lapply(., is_in_range, x=result[ ,'time']) %>%
    lapply(., function(subs) result[subs, , drop=FALSE]) %>%
    lapply(., as.data.frame) %>% 
    mapply0(mutate, ., range=names(time_ranges)) %>%
    do.call(rbind, .) 
}

is_in_range <- function(range,x) x >= min(range) & x <= max(range)

# Adjust the names of a result (mat or df)
adjust_names <- function(result, prefix="sp") { 
  cols <- colnames(result)
  cols[is_nodecol(cols)] <- paste0(prefix, cols[is_nodecol(cols)])
  colnames(result) <- cols
  
  return(result)
}

# Currently tests if single digit with possible prefix 
is_nodecol <- function(current.names, prefix="") { 
  grepl(paste0('^(',prefix,'|)[0-9]{1}$'), current.names)
}
