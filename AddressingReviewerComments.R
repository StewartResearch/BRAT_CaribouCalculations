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

#default BRAT
BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.9,pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335 # This is the lambda Quatile
# This is the top event lambda: 0.935
# [1] FALSE # this means that the top event lambda is less than (1-lambda quartile +1)


#vary sd1
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.15, multiplier1 = 0.9, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.038002
# This is the top event lambda: 0.935
# [1] FALSE
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.05, multiplier1 = 0.9, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.012667
# This is the top event lambda: 0.935
# [1] FALSE


## vary multiplier1 ##
BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 1.0, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE

BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.45, pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE


## vary pregR ##
BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.9,pregR = 1.0, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE
BRAT(N = 250,SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.45,pregR = 1.0, sexRatio = 0.5, 
     wolfCullEffect = 0.186, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE

## vary wolfcullEffect ##
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.9,pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.279, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE
BRAT(N = 250, SadF = 0.87,recr = 0.13, sd1 = 0.1, multiplier1 = 0.9,pregR = 0.9, sexRatio = 0.5, 
     wolfCullEffect = 0.903, wolfCullPropOnAdults = 0.1)
# [1] 1.025335
# This is the top event lambda: 0.935
# [1] FALSE





# Response
# Answer a question: By how much would each threat line need to change in order to either increase or decrease the Current
# Total Top Event Lambda by 5%?

# source("CaribouCalculations_Final_Final.R")
# 
# #Calculate the optimazation on LOPA factor values, for each threat line
# initialFrequency<-as.vector(c(Threat1_InitialFreq, Threat2_InitialFreq, Threat3_InitialFreq, Threat4_InitialFreq))
# barriers<-(c(as.vector(sum(unlist(sapply(Threat1_barriers, eval)))), Threat2_barriers, Threat3_barriers, Threat4_barriers))
# 
# optim(par = initialFrequency, fn =  topEventCalculator(threatCalculator), method = "L-BFGS-B", lower = (topEvent - 0.05), upper = (topEvent + 0.05))
# 
# 
# # now translate this into lambda values
# # optim_lambdaUpper <-round(((1 - optim) + 1), 3)
# 
# # Do for other threat lines too. 
      