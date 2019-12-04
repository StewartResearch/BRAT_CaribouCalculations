#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# Started: January 2019
# Last revised: November 2019
# authors: Eliot McIntire and Frances Stewart
#################################################################################

######################
# Frontiers in Ecology and Evolution
# Editor: Michael Morrison
# Reviewer 2 comments
# Revision 5
# November 26, 2019

# Nor was there any form of sensitivity analysis. None of the LOPA assignments were varied to learn of 
# each threat’s contribution to the risk event in the face of uncertainty associated with each threat. 
# But more importantly, only one validation of a model prediction is presented, and this validation is vague. 
# The model’s risk prediction is said to be in agreement with the government’s risk prediction (at one of the 
# three study sites), but the government’s risk prediction is not presented. Also missing from this validation 
# was any mention of an acceptable level of precision accompanying the model’s prediction.


#variables that we can change - others are from literature and do not necssarily have:
## sd1 - this was initially 0.1 (from Sutherland sensitivity analysis)
## multiplier 1 - this was initially 90% (90% of adult female mortality is due to Threat line 1)
## wolfCullEffect - this is from Hervieux et al 2014 and could change with more information <- 0.186
## wolfCullPropOnAdults - we assumed that the majority of a wolf cull evect ws on juveniles <- 0.1


# December 3

# three types of unceratinty/error that we could look at:

# FIRST: estimation error of the existing barriers in the BRAT:
# default BRAT ----
BRATcalc(0.055, c(1.716, 1.40, 0.65, 1.5, 1.0, 0.9, 1.0), 
         0.687, c(1.325), 
         0.019, c(1.0, 1.0, 1.0), 
         0.016, c(1.05, 1.05, 1.05, 1.0, 1.0))
# [1] 1.025335
# This is the top event lambda: 0.936
# [1] FALSE

# Change each component one by one:
BRATcalc(0.055, c(0.858, 1.4, 0.65, 0.75, 1.0, 0.9, 1.0), 
         0.687, c(1.325), 
         0.019, c(1.0, 1.0, 1.0), 
         0.016, c(1.05, 1.05, 1.05, 1.0, 1.0))


# SECOND: uncertainty around the standard deviation of the lambda curve from which we draw the lambdaQuartile
#default BRAT function ----
BRAT(N = 250,SadF = 0.87, recr = 0.13, sd1 = 0.1, multiplier1 = 0.9, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335 # This is the lambda Quatile
# This is the top event lambda: 0.935
# [1] FALSE # this means that the top event lambda is less than (1-lambda quartile +1)

#vary sd1
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.15, multiplier1 = 0.9, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.038002
# This is the top event lambda: 0.935
# [1] FALSE
BRAT(N = 250, SadF = 0.87, recr = 0.13, sd1 = 0.05, multiplier1 = 0.9, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.012667
# This is the top event lambda: 0.935
# [1] FALSE


# THIRD: uncertainty around the weighting of the threat lines
## vary multiplier1 ##
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 1.0, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE

BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.8, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE

## FOURTH: calculating juvenile predation inital frequency
# vary the surv, pregR, and SadF to investigate the sensitivity of Threat2_InitialFrequency
BRAT(N = 250, SadF = 0.89, recr = 0.13, sd1 = 0.1, multiplier1 = 0.9, surv = 0.8, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)




# ############################################
# # try mcmc sampling each of the barrier values
# m <- 1.716 # mean
# s <- 1 # standard deviation
# # assume a normal distribtuion for each of the barriers
# set.seed(1)
# samples<-rnorm(10000, m, s)
# 
# cummean<- function(x){
#         cumsum(x)/seq_along(x)
# }
# 
# plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
#      panel.first=abline(h=0, col="red"), las=1)
# 
# for (i in seq_len(30))
#         lines(cummean(rnorm(10000, m, s)),
#               col=rgb(runif(1), runif(1), runif(1), .5))
# 
# summary(samples)
# p <- 0.025
# a.tru <-qnorm(p,m,s)
# #estimate the point that the 2.5% probability density is below using mcmc:
# a.mc<-unname(quantile(samples,p))
#       