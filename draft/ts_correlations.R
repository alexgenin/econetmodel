
# Analyze time-series correlations and do stuff

df <- as.data.frame(result)

cormats <- dlply(df, ~ N, function(dat) {
                  cor(df[ ,c('node1','node2','node3','node4','node5')])
                  })


cormat_mean <- Reduce(`+`,cormats) / length(cormats)

image(cormat_mean)

library(igraph)
library(qgraph)
library(scales)
corgraph <- qgraph(cormat_mean, graph='cor') 
plot(corgraph)
