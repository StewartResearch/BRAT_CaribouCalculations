This code corresponds to publication:
Winder, R, F.E.C. Stewart, S. Nebel. E.J.B. McIntire, A. Dyk, and K. Omendja. 2020
Cummulative effects and boreal woodland caribou: How bow-tie risk analysis addresses a critical issue in Canada's forested landscapes.
Frontiers in Ecology and Evolution, doi: 10.3389/fevo.2020.00001


# BRAT_CaribouCalculations
R code describing how to calculate the target frequency for a BRAT critical event given our knowledge of caribou recruitment and survival

This code emulates the BowtieXP software use for Bowtie Risk Analysis Tool (BRAT).

###
###
To run the SpaDES module for this BRAT, go to the modules folder then run the BRATframework.Rmd file
###
###

Otherwise:
########################################################
Use the BRATfunction.R script to run the left hand side of the BRAT for each specific caribou herd/study area of interest. This function
requires knowledge about the population size, adult female survival, and recruitment of specific caribou herds. It makes assumtions (which you can specify as parameter values) of the standard deviation of the lambda curve, weight assigned to each threat line, pregancy rate, calf survival to day 1, calf tertiary sex ratio (i.e. at recruitment), the effect of a wolf cull, and proportion of wolf cull's effect on adults versus juveniles. 

The BRATfunction can also be used to test out the sensitivity of a BRAT, by altering one or more parameters within the function, and observing how the top event lambda value changes. Alternatively, you can alsu use the BRATcalculator.R function for this purpose. 

########################################################
Use the Mitigationfunction.R to calculate the right hand side of the BRAT. This function spefifies parameter values for each mitigation option (in the case of Winder et al. 2020 Frontiers in Ecol & Evol, these mitigation options are the wolf cull, seismic trace restoration, and maternal penning). These options can be changed to reflect the recommended mitigation option for each study area/herd. This function calculates the consequence frequency if all, one, or multiple mitigation options are deployed. 

########################################################
Finally, you can use the PLOTfunction.R to plot a very rough, rbase plot, version of the BRAT diagram. This plot will produce the values of each threat, barrier, and top event/consequence frequency. The code can be changed to reflect the BRAT function written for your study area/herd. The plot will fit to the window size of your screen, so make sure to Zoom in on the plot if using RStudio.
