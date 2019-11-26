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

# Response
# Answer a question: By how much would each threat line need to change in order to either increase or decrease the Current
# Total Top Event Lambda by 5%?

source("CaribouCalculations_Final_Final.R")

#Calculate the optimazation on LOPA factor values, for each threat line
initialFreq<-as.vector(c(Threat1_InitialFreq, Threat2_InitialFreq, Threat3_InitialFreq, Threat4_InitialFreq))
barriers<-as.vector(c(sapply(Threat1_barriers, eval), Threat2_barriers, Threat3_barriers, Threat4_barriers))

optim(par = initialFreq, fn =  threatCalculator(initialFreq,barriers), method = "L-BFGS-B", lower = (topEvent - 0.05), upper = (topEvent + 0.05))




# now translate this into lambda values
# optim_lambdaUpper <-round(((1 - optim) + 1), 3)
      