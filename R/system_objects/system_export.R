# 
# Functions that take a system object and return understandable stuff
# 
  
# Select time range(s) (so unreadable)
select_ranges_ <- function(result, time_ranges) { 
  time_ranges %>%
    lapply(., .is_in_range, x=result[ ,'time']) %>%
    lapply(., function(subs) result[subs, , drop=FALSE]) %>%
    lapply(., as.data.frame) %>% 
    mapply0(mutate, ., range=names(time_ranges)) %>%
    do.call(rbind, .) -> 
    newdat
  
  # So slow though
  range.order <- seq_along(names(time_ranges))
  names(range.order) <- names(time_ranges)
  
  newdat[ ,"range"] <- with(newdat, reorder(range, range.order[range]))
  
  return(newdat)
}
select_ranges <- make_dotted(select_ranges_)
.is_in_range  <- function(range,x) x >= min(range) & x <= max(range)

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
