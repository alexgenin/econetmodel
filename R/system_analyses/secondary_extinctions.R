# 
# Functions to help analysings secondary extinctions
# 

compute_stats <- function(result, 
                          range1, range2, fun.list, 
                          sys=attr(result, 'system'),
                          ...) {
  
  results <- lapply(fun.list, function(fun, ...) { 
                      apply_two_ranges(fun)(result, range1, range2, ...)
                    }, ...)
  
  with_system_attr(as.data.frame(results), sys)
}

stats_effect_biomass <- function(...) { 
  output <- stats_effect_sizes(...) # this is pure lazyness but it works
  if (all(is.na(output))) { 
    return(data.frame(tbiomass=NA_real_))
  } else { 
    return(data.frame(tbiomass=sum(output, na.rm=TRUE)))
  }
}

stats_effect_sizes_bytroph <- function(..., trophlvls) { 
  output <- stats_effect_sizes(...) # this is pure lazyness too 
  
  if (all(is.na(output))) { 
    output <- as.list(rep(NA_real_, length(unique(trophlvls))))
    names(output) <- unique(trophlvls)
  } else { 
    output <- t(tapply(t(output), trophlvls, FUN=mean, na.rm=TRUE)) # beurk
  }
  
  return(as.data.frame(output))
}

stats_effect_sizes <- function(range1_dat, range2_dat, 
                               species_cols, removal_col, 
                               ...) { 
  # Get column names
  removal_col <- dplyr::select_vars_(colnames(range1_dat), substitute(removal_col))
  species_cols <- dplyr::select_vars_(colnames(range1_dat), substitute(species_cols))
  
  # Get biomass differences
  delta_biomass <- (range2_dat[ ,species_cols] - range1_dat[ ,species_cols]) / 
                      range1_dat[ ,species_cols]
  removed_spp   <- seq.int(length(species_cols)) %in% rmcase2vec(range2_dat[ ,removal_col])
  
  delta_biomass[removed_spp] <- NA
  as.data.frame(delta_biomass)
}

stats_sec_extinctions <- function(range1_dat, range2_dat, 
                                  species_cols, removal_col, 
                                  ...) { 
  
  # Get column names
  removal_col <- dplyr::select_vars_(colnames(range1_dat), substitute(removal_col))
  species_cols <- dplyr::select_vars_(colnames(range1_dat), substitute(species_cols))
  
  # A species is secondary extinct if its abundance is 0 but it has not been 
  # removed artificially.
  is_extinct <- lapply(list(range1_dat, range2_dat), 
                  function(range) range[ ,species_cols] == 0)
  n_extinct <- sum(is_extinct[[2]]) - sum(is_extinct[[1]])
  
  n_removed <- nchar(as.character(range2_dat[ , removal_col]))
  
  return(data.frame(secextinct=n_extinct - n_removed))
}

apply_two_ranges <- function(fun, 
                             range_col='range',
                             aggregate.fun=colMeansFactor) { 
  
  function(result, range1, range2, 
           ...) { 
    ranges_dat <- lapply(list(range1, range2), 
                        function(range) { 
                          dat <- result[result[ ,range_col] == range, , drop=FALSE]
                          if (nrow(dat)>1) {
                            dat <- aggregate.fun(dat)
                          }
                          dat
                        })
    
    # Get data
    range1_dat <- result[result[ ,range_col] == range1, ]
    range2_dat <- result[result[ ,range_col] == range2, ]
    
    # 
    fun(ranges_dat[[1]], ranges_dat[[2]], ...)
  }
  
}

colMeansFactor <- function(df) { 
# Make a special case for factors and strings: if all equals, then we set it 
# to the single value. 
  dat <- lapply(df, 
                function(col) { 
                  if (is.numeric(col)) mean(col, na.rm=TRUE) else col[1] 
                })
                
  as.data.frame(dat)
  
}

removal_cases <- function(result, removal_cols) { 
  case <- ''
  for (i in seq.int(8)) { 
    case <- paste0(case, ifelse(dat[, paste0('removedspecies',i)] > 0, i, ''))
  }
  dat[ ,'removal'] <- case
  dat
}

rmcase2vec <- function(rmcase) { 
  as.numeric( unlist(strsplit(as.character(rmcase), '')) )
}
