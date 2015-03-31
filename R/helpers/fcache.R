# 
# Fcache is a memoizer for functions. It analyses a call and its arguments
# and if the result is alread computed, then it will retrieve it form disk.
# 
# 

FC <- fcache <- function(fun,...,
                         cache.dir='.cache',  # caching directory
                         cache.ignore=FALSE,  # ignore cache, always compute result
                         cache.clear=FALSE,
                         verbose=FALSE) {  # other args passed to function
  if (!require(R.cache)) 
    stop("fcache needs package R.cache to work.")
  
  if (cache.clear) {
    message('Removing cache files...\n')
    sapply(dir(cache.dir,pattern='Rcache',full.names=TRUE), file.remove)
    return(invisible(NULL))
  }
  
  if ( ! file.exists(cache.dir)) { 
    dir.create(cache.dir)
  }
  
  # Set the root directory for cache
  R.cache::setCacheRootPath(cache.dir)
  
  # Compute cache key by eval'ing all arguments and the passed function itself.
  # => If either the function or one of the call arguments has changed, the 
  # result is recomputed.
  args.all    <- as.list(match.call(expand.dots=FALSE))
  passed.args <- lapply(as.list(args.all[['...']]),eval,envir=parent.frame())
  passed.fun  <- deparse(eval(fun)) # we work on the character representation
  cache.key   <- list(fun=passed.fun, args=passed.args)
  
  # Build subdirectory corresponding to function name
  passed.fun.name <- as.character(args.all$fun)
  
  if (verbose) {
    message('Looking up result for ',as.character(args.all[[2]]),
            ' [key:',digest::digest(cache.key),']',sep='')
  }
  
  fun.result <- NULL  # init
  
  # Try to load cache for given expression
  if (!cache.ignore) {
    fun.result <- R.cache::loadCache(cache.key, dirs=passed.fun.name)
  }
  if (verbose && !is.null(fun.result)) {
    message(' -- OK\n')
  }
  
  # Compute result if no cached result is available
  if (is.null(fun.result)) {
    if (verbose) cat(' -- Nothing found\n')
    fun.result <- fun(...)
    R.cache::saveCache(fun.result, key=cache.key, dirs=passed.fun.name)
  }
  
  return(fun.result)
}


# This function returns the same function, but with memoizer enabled.
with_fcache <- function(fun) { 
  function(...) { 
    fcache(fun, ...)
  }
}
