# 
# This file will read the results from the simulation analyses, then display 
# it in a figure. 
# 

# Package dependencies
library(ggplot2)
library(gridExtra)
library(svglite)
# library(devtools)


# Set working dir
wd <- "/home/alex/work/2014-2015/network_modelling/dev/examples/"
setwd(wd)

load("sensitivity_data.rda", verbose = TRUE)


# Build a new variable testing only for the extinction status
sens_results$status_extinctions <- 
  with(sens_results, { 
         tmp <- cbind(ifelse(has_extinctions_w,  'Whelks',  NA), 
                      ifelse(has_extinctions_wg, 'Grazers', NA))
         tmp <- apply(tmp, 1, function(X) paste(na.omit(X), collapse = " and "))
         tmp[grepl("^ and $", tmp)] <- "No extinctions"
         tmp[unstable] <- "Unstable network"
         tmp[grepl("^$", tmp)] <- "No extinctions"
         tmp
       }) 

# Order levels to make sure the colors are right in the graph
sens_results$status_extinctions_ <- factor(sens_results$status_extinctions, 
                                      levels = c("Unstable network", 
                                                 'No extinctions', 
                                                 'Whelks', 
                                                 'Whelks and Grazers'), 
                                      ordered = TRUE)
# Nice levels for figure
levels(sens_results$status_extinctions_) <- c('Unstable network', 
                                        'No extinctions', 
                                        'Extinctions occur after predator removal only', 
                                        'Extinctions occur after predator and grazers removal')


# Build a new variable that looks at changes in taxa/biomasses as we remove 
#   *one* predator
sens_results$status_biomasses <- 
  with(sens_results, { 
      tmp <- cbind(ifelse(has_extinctions_w, 'Algae extinctions', NA),
                   ifelse(has_extinctions_w & diff_mussels_ok & diff_grazers_w_ok, 'Consumer biomass', NA))
      tmp <- apply(tmp, 1, function(X) paste(na.omit(X), collapse = " + "))
      tmp[grepl("^ +\n $", tmp)] <- "No extinctions"
      tmp[unstable] <- "Unstable network"
      tmp[grepl("^$", tmp)] <- "No extinctions"
      tmp
  })

sens_results$status_biomasses_ <- factor(sens_results$status_biomasses, 
                                    levels = c("Unstable network", 
                                               'No extinctions', 
                                               'Algae extinctions', 
                                               'Algae extinctions + Consumer biomass'), 
                                    ordered = TRUE)
levels(sens_results$status_biomasses_) <- c('Unstable network', 
                                            'No extinctions', 
                                            'Presence of extinctions only', 
                                            'Presence of extinctions and correct variations in consumers\' biomasses')

# Make a parsable label for a0
sens_results[ ,'a0_parsable'] <- paste0("a[0]:", sens_results[ ,'a0'])

# Define some color scales
scale_extictions <- scale_fill_manual(values = c("#EEEEEE", "#B0B0B0",
                                                 "#D0E83A", "#E8643A"),
                                      name = "Occurrence of extinctions")
scale_biomasses <- scale_fill_manual(values = c("#EEEEEE", "#B0B0B0",
                                                "#26A9E2", "#E8AD3A"),
                                     name = "Variations in biomasses")

# Add a point corresponding to the used parameters
used_parms <- data.frame(a0 = 0.05,
                         a0_parsable = paste0("a[0]:", 0.05),
                         c =   .3,
                         fp = .4)
used_par_annot <- geom_point(aes(x = fp, y = c), 
                             data = used_parms, 
                             shape = 18, size = 4)


plot_extinctions <- 
  ggplot(sens_results) + 
    geom_tile(aes(x = foragpref_strength, 
                    y = compet, 
                    fill = status_extinctions_)) + 
    used_par_annot + 
    facet_grid( ~ a0_parsable, labeller = "label_parsed") + 
    xlab(expression(paste("Strength of foraging modulation ",s[fp])) ) + 
    ylab('Competition strength c') + 
    scale_extictions + 
    theme_minimal() + 
    theme(legend.position  = "bottom", 
          legend.direction = 'vertical')
plot_extinctions

# dev.new()
plot_biomasses <- 
  ggplot(sens_results) + 
    geom_tile(aes(x = foragpref_strength, 
                    y = compet, 
                    fill = status_biomasses_)) + 
    used_par_annot + 
    facet_grid( ~ a0_parsable, labeller = "label_parsed") + 
    xlab(expression(paste("Strength of foraging modulation ",s[fp])) ) + 
    ylab('Competition strength c')  + 
    scale_biomasses + 
    theme_minimal() +
    theme(legend.position = 'bottom', 
          legend.direction = "vertical") 

both_figs <- grid.arrange(plot_extinctions, 
                          plot_biomasses, 
                          ncol = 1)

ggsave(plot = both_figs ,
       filename = "figure_raw_sensitivity.pdf", width = 10, height = 11)
