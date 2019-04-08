# 
# 
# This file contains code that will produce a bar chart illustrating the 
#   behavior of the model according to the inclusion of NTIs
# 

library(ggplot2)
library(tidyr)


# Set working dir
wd <- "/home/alex/work/2014-2015/network_modelling/dev/examples/"
setwd(wd)


load('./simu_data.rda', verbose = TRUE)

# Adjust labels to reflect naming in the manuscript
levels(result_delta[ ,'simu_fmt']) <- gsub("Predatory avoidance", 
                                           "Consumer foraging modulation", 
                                           levels(result_delta[ ,"simu_fmt"]))
levels(result_extinctions[ ,'simu_fmt']) <- gsub("Predatory avoidance", 
                                           "Consumer foraging modulation", 
                                           levels(result_extinctions[ ,"simu_fmt"]))


# Plot that shows extinctions and increase/decrease in mussels abundance
plot <- ggplot(subset(gather(result_delta, sp, delta, mussels, grazers),
                      time == max(time) & delta > -1 & 
                      rm1 == 'whelks' & rm2 == '-')) + 
  geom_bar(aes(x = simu_fmt, 
               y = round(delta * 100), 
               fill = sp), stat = 'identity',
           position = 'dodge') + 
  geom_point(aes(x = simu_fmt, y = 45), 
             data = subset(result_extinctions, extinctions), 
             shape = '*', size = 10, color = 'red') +
  scale_fill_manual(name = 'Species', 
                    values = c('#FF8000', '#6193CF')) + 
  ylab('Relative change in abundance (%)') + 
  xlab('Set of interactions included') + 
  ggtitle('One top predator removed') + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

plot

ggsave(plot = plot, 
       filename = 'figure_results_nti_inclusions.pdf', 
       width = 6, height = 5)

