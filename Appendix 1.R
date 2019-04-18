#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# Code that outlines the appendix - maybe make into an Rmd?
# Started: April 2019
# Last revised: April 18, 2019
# Author: Frances Stewart
#################################################################################


# draw a simple normal distribution, with mean = 1 and sd = 0.1, as in the manuscript
# to plot the lambda values on for the Appendix lambda figure

install.packages("cowplot")
library(cowplot)

# A) Section 2.6.1.3 Threat 1, Barrier 3: Wolf trapping and hunting
p1 <- ggplot(data = data.frame(x = c(-1, 1)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 0.1), geom = "area") + ylab("") + 
  scale_y_continuous(breaks = NULL) +
  xlab("lambda")
p1
# add sd vertical lines to this plot
p2 <- p1 + geom_vline(xintercept = 0.1, linetype = 'dashed', size  = 0.6, colour = "White") + geom_vline(xintercept = -0.1, linetype = 'dashed', size = 0.6, color = "white")
p2
# add a line for the lambda value
p3 <- p2 + geom_vline(xintercept = -0.019, linetype = 'dashed', size  = 1, colour = 'red')
p3
ggsave(p3, plot = ggplot2::last_plot(), device = NULL)


# B) Section 2.6.1.4 Threat 1, Barrier 4: Management of early seral stage forests for cover to reduce access
g1 <- ggplot(data = data.frame(x = c(-1, 1)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 0.1), geom = "area") + ylab("") + 
  scale_y_continuous(breaks = NULL) +
  xlab("lambda")
g1
# add sd vertical lines to this plot
g2 <- p1 + geom_vline(xintercept = 0.1, linetype = 'dashed', size  = 0.6, colour = "White") + geom_vline(xintercept = -0.1, linetype = 'dashed', size = 0.6, color = "white")
g2
# add a line for the lambda value
g3 <- p2 + geom_vline(xintercept = 0.027, linetype = 'dashed', size  = 1, colour = 'red')
g3
ggsave(p3, plot = ggplot2::last_plot(), device = NULL)
