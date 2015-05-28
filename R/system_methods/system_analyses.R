
effect_size <- function(result, time.a, time.b,
                        sys=attr(result, 'sys')) { 
  
  
  state_a <- result[last_before(time.a, result[ ,'time']), 
                    seq.int(get_size(sys))+1]
  state_b <- result[last_before(time.b, result[ ,'time']), 
                    seq.int(get_size(sys))+1]
  
  sum(abs(state_b - state_a))
}

insert_total_biomass <- function(result, 
                              sys=attr(result,'sys')) { 
  
  result[ ,'tbiomass'] <- sum(result[ , seq.int(get_size(sys))+1])
  
  with_system_attr(result, sys)
}

