

# Tests functional response type 2 ------------------------

preyab <- c(0,5,10,20)

fr <- sapply(X=preyab, 
             function(ab) { 
               params <- list(X   = c(ab,1),
                              atk = matrix(c(0,0,
                                             1,0), byrow=TRUE, ncol=2),
                              h   = c(1,1))
                              
                              do.call(fresp_type2,params)[2,1] # effect on predator 2 on prey 1
                              })

# plot.dat <- data.frame(preyab, fr)
# ggplot2::qplot(preyab,fr,geom='line', data=plot.dat)

# Check answers
answers <- c(0, 0.833333333333333, 0.909090909090909, 0.952380952380952)
for (i in seq.int(length(fr))) {
  expect_equal(fr[i], answers[i])
}
