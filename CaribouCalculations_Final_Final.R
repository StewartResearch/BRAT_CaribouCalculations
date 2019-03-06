#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# February 2019
# By Eliot McIntire and Frances Stewart
#################################################################################

# Objectives: ----
### Calculate target frequency for the critical event
##### Policy says the target frequency should be 40% (Environment and Climate Change Canada 2012 Recovery Strategy) 
## - ECCC has indicated landscape change should not exceed 35%, as this produces a 60% probability of herd persistence. 
## In other words, this policy threshold is stated at 40% risk of population decline for any specific population.
#But this 40% is obtained from a normal distribution 
## - i.e. it is the area under a normalized curve (bc the models in ECCC2012 assume a normal distribution)
## representing the lower 40% of the distribution.

## Overall, we want to determine if a herd is above the 40% threshold (ie in the 60% probability of survival category)
## We therefore set 60% as our target threshold to achieve.


###########################
###########################
# Chinchaga herd (STUDY AREA 1) - DONE ---- 
###########################

#########
# Set the barriers here -- this would be the numbers that go into the BRAT figure
Threat1_barriers <- c(quote(Threat1_barrier_1), 1.4, 0.65, 1.5, 1.0, 0.9, 1.0) # in units of Initial Frequency
Threat2_barriers <- quote(c(Threat2_barrier_1)) # in units of Initial Frequency # This is derived from data below
Threat3_barriers <- c(1.00, 1.00, 1.00) # in units of Initial Frequency
Threat4_barriers <- c(1.05, 1.05, 1.05, 1.00, 1.00)

###############################################################################
# Step 1: know (or estimate) some basic information about each study area/herd. 
### average demographic values between snake and Chinchaga as of 2008 - from downloadable csv online:
# source: https://open.canada.ca/data/en/dataset/4eb3e825-5b0f-45a3-8b8b-355188d24b71

# Chinchaga

# Step 1: know (or estimate) some basic information about each study area/herd. 
### For Chinchaga: data obtained from the ECCC 2011 scientific report
N = 250 # population of the Chinchaga herd as of 2008
SadF = 0.87 # Adult female survival 
recr = 0.13/2 # Juvenile female recruitment 
sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

# invert these numbers to calculate the mortality values:
MortSadF = (1 - SadF) # part of this will go into the Initial frequency of the threats
Mortrecr = (1 - recr) # part of this will go into the Initial frequency of the threats, only 1/2 the animals are female



################################
# Step 2: State your assumptions
### In this example, there are 4 threats. 
### Three of these threats apply to adult females, and three of these threats apply to juveniles
### One threat each applies to only adult females and juveniles
##### We need to separate out a portion of the above mortality measures for each of the frequencies 
## (whether initial, or current top event) for each threat.

##### The literature generally says that most of the mortality is due to predation, 
## and that the majority of this is on juveniles (Hervieux et al. 2014; Wittmer et al. 2005 a and b, etc). 
#####

Threat1_multiplier = MortSadF * 0.90 # ASSUMPTION # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
#Threat2_multiplier = We calculate this amont below, and name it 'Difference_nonpredation', so we do not include it here.
Threat3_multiplier = MortSadF * 0.05 #+ Mortrecr * 0.05 # ASSUMPTION # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
Threat4_multiplier = Threat3_multiplier # ASSUMPTION

# Assumptions about caribou population demongraphics, from published literaure, reports, and pers. comms:
# These values are used to calculate the current top event frequency, for juveniles
# pregnancy rate
pregR <- 0.9 # DATA/ASSUMPTION # pregnancy rate - from observations in maternal pens 
# (Scott McNay pers com, Kinse-Za maternal penning project)
# juvenile survival to the fist day of life
sexRatio<- 0.5 # only half of the animals born will be female 
surv1stDay <- 0.8 * sexRatio # DATA/ASSUMPTION # juvenile female survival to the first day - from maternal pen observations 
# (Scott McNay pers com, Klinse-Za maternal penning project)
mort1stDay <- 1 - (surv1stDay)  

###########################################################################################################
# Step 3: calculate a normal distribution, and indicate the 60% value
### This 60% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
### In other words, we are OK with 40% of the herds dissapearing, on average.

lambdaQuartile <- qnorm(0.6, mean = 1, sd1) # mortality quartile is at the 60% mark of this distribution. 
# in other words, what lambda vlaue do herds need to accomplish to obtain sustainability according to ECCC's 2012 40% threshold.

# Put this lambdaQuartile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print(lambdaQuartile) 
# 1.02

##################################################################################
# Step 4:populating the BRAT framework Initial frequency and Current top events of threats
# Step 4 thorugh 6 are summarized in Table 2 of the manuscript (Winder et al. 2019)

# Determining what vlaues should go into the Initial Frequency of a Threat (Blue boxes), and what values should 
# go into the barriers to a threat (White boxes) can be tricky and subjective in the BRAT framework. 
# The values need to come from the best sourced data, and when those data are limited, they  can be hard to determine.

# However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
# available. Our manuscript here is show how, and can be changed as new information comes to light - not only the number 
# of threats or barriers to a threat, can be changed, but the associations between these items can be changed too.

####### Current top event frequencies are from known data 
## (ie. known mortality or recruitment from ECCC data)
####### Initial event frequencies are things that we need to find out (i.e. calculate)
## (ie. what was the frequency of this threat if there had been no landscape changes)


# Threat 1 - General predation
### Current Top Event Frequency
# some herds have very low adult female mortality. Thorugh trial and error with Snake and the Averaged herds, we discovered that
# if MortSadF is less than 0.1 (i.e less than 10% of adult females are dying) the effectiveness of wolf controls was listed as > 100%.
# this just didnt make any sense. We therefore assume # ASSUMPTION # that 10% represents an ecological threshold of other processes
# dominating adult female survival. We discovered that if we limit the initial frequency of adult female mortality to only 40%
# by predation, the effectiveness of wolf controls made sense (i.e. less than 100%) across all study situations. We therefore assume
# that if less than 10% of females are dying, 4% will be due to wolf predation, and the other 6% will be due to other causes (highlighted
# in threats 3 and 4).
if(MortSadF < 0.10) {
  Threat1_InitialFreq <- MortSadF*0.4

  } else { # if not, we back calculated the required threat value:
Threat1_InitialFreq <- 0.05549 # ASSUMPTION
# This calculation involved back calculating from the wolf mitigation initial frequency, to the juvenile predation threat frequency,
## translating this value into lambda units of wolf predation and compensatory predation, calculating the ratio between these
## predation effects, and applying the ratio to adult wolf vs compensatory predation. From there we could substitute in the current
## top event frequency (known from ECCC data), to get this inital frequency value.
# ASSUMPTION: the effectiveness of wolfculls extends from juveniles to adults equally
# ASSUMPTION: This is a constant applied to all populations to determine the threat values of adult predation (as long as adult female
# mortality is > 10%. See above portion of the IF statement). 
### This might be a big assumption! But it is mitigated by the above IF statement.
###### See how we calculated this at the end of the script.
}


Threat1_barrier_1 <-  Threat1_multiplier / (Threat1_InitialFreq * prod(unlist(Threat1_barriers[-1])))
Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality
Threat1_InitialFreq <- Threat1_topEvent/prod(sapply(Threat1_barriers, eval))

### Initial Frequency
##### calculated from the BRAT threat line:
# predationAdultRev <- function(Threat1_InitialFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
#   TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
# }


# Threat 2 - Juvenile predation
### Initial Frequency
##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is a factor)
# Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
Threat2_InitialSurv <- pregR * SadF * surv1stDay # proportion of females that produce a female  calf
Threat2_InitialFreq <- (1 - (Threat2_InitialSurv)) # proportion of females at the start of a year that result in no female calf a year later

### Current Top Event Frequency
##### Difference between the calculated Initial Frequency, and the ECCC reported value (i.e. how 
#  many female calf deaths from day 1 to a population survey are unaccounted for?)
PropMortDay30ToSurvey = Mortrecr - Threat2_InitialFreq 
# We know from maternal penning data that even between day 2 and day 30, 90% of calves survive 
#   (i.e. even without predation, 90% of calves survive, and 10% die from "other causes"; Scott McNay pers comm - Klinse-Za meternity pen)
## Assume this rate is consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
PropMortDay30ToSurvey_predation <- (PropMortDay30ToSurvey * 0.9) # DATA/ASSUMPTION #
# Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
Threat2_topevent <- PropMortDay30ToSurvey_predation + Threat2_InitialFreq 

### Barrier value: Effect of predator avoidance
##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
Threat2_barriers <- Threat2_topevent / Threat2_InitialFreq
Threat2_barrier_1 <- Threat2_barriers[1]


# Needed for the rest of the framework:
### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
Difference_nonpredation = PropMortDay30ToSurvey - PropMortDay30ToSurvey_predation
### We need split this rate to the final two Threats, to account for the proportion of non-predation mortality
# directly attributable to female juveniles


# Threat 3: Permanent habitat appropriation
### Current Top Event Frequency
Threat3_topevent = Threat3_multiplier + (0.5 * Difference_nonpredation) # DATA/ASSUMPTION #### ----

### Initial Frequency
# multiply it by what values exist in the barrier to backcalculte this value
Threat3_InitialFreq <- Threat3_topevent/(prod(Threat3_barriers))


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier + (0.5 * Difference_nonpredation) # DATA/ASSUMPTION #### ----
# assumed mortality proportion for adult females, plus, what we know of juvenile mortality minus 50% of what we dont know about juvenile mortality

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
# Threat4_InitialFreq <- Threat4_multiplier*(prod(0.95, 0.950, 0.95, 1.00, 1.00))
Threat4_InitialFreq <- Threat4_topevent/(prod(Threat4_barriers))

#######################################################
# step 5: Convert the inital frequency values to values of lambda:

Threat_LambdaEffect <- list()
Threat_LambdaEffect[[1]] <- (Threat1_InitialFreq * sapply(Threat1_barriers, eval)) - Threat1_InitialFreq # in Lambda units
Threat_LambdaEffect[[2]] <- Threat2_InitialFreq * Threat2_barriers - Threat2_InitialFreq # in Lambda units
Threat_LambdaEffect[[3]] <- Threat3_InitialFreq * Threat3_barriers - Threat3_InitialFreq # in Lambda units
Threat_LambdaEffect[[4]] <- Threat4_InitialFreq * Threat4_barriers - Threat4_InitialFreq # in Lambda units

wolfCullEffect <- 0.186 # DATA/ASSUMPTION 
# i.e. lambda has the potential to increase by 18.6% during a full wolf cull.
# Hervieux et al. (2014) found lambda increased between 4.6 (within herds) and 14% (between herds) during a 50% wolf cull. 
# We avereaged these fundings, and standardized by 50% wolf cull to get the 18.6%
# this assumes a linear relationship

#############################################################################################################################
# Step 5b
# we now wanted to stplit this up between the effect of predation by wolves, and the effect of predation as a compensatory effect 
# (or "other" predators, OR wolves that are not being culled)
# i.e. this could include wolf immigration and source/sink dynamics -> an important discussion point.

# We wanted to state this in terms of lmabda units
Threat_LambdaEffect[[2]]
#  So, 0.444 is predator effect on calves -- partition into "non-compensated wolf" == "True effect of wolves on calves" 
#     and "other" e.g., 0.186 is max possible... partition amongst adults and juvs e.g., 0.9 and 0.1 -->
#     0.9 * 0.186 on calves and 0.1 * 0.186  on adults
#  "other predation" for adults = 0.08775 - (0.1 * wolfCullEffect) = 0.07375 in Lambda units
#     i.e. Threat_LambdaEffect[[1]] - (0.1*wolfcullEffect)
#  "other predation" for adults = 

# ASSUMPTION # we assumed that the majority of predation happend on juveniles (Hervieux et al. 2014)
wolfCullPropOnAdults <- 0.1
wolfCullPropOnJuvs <- 1 - wolfCullPropOnAdults
# therefore: 
wolvesOnAdults <- wolfCullPropOnAdults * wolfCullEffect # Lambda units
otherOnAdults <- Threat_LambdaEffect[[1]][1] - wolvesOnAdults # Lambda units

# Percent effectiveness on adult recruitment:
effectivenessAdults<- wolvesOnAdults/(otherOnAdults + wolvesOnAdults)


#  "other predation" for calves = 0.444584 - 0.9 * wolfCullEffect = 0.277 in Lambda units
#     i.e. Threat_Lambda[[2]] - (0.1*wolfcullEffect)
#  "other predation" for calves = 
wolvesOnJuvs <- wolfCullPropOnJuvs * wolfCullEffect # Lambda units
otherOnJuvs <- Threat_LambdaEffect[[2]][1] - wolvesOnJuvs # Lambda units

# Percent effectiveness on juvenile recruitment:
effectivenessJuvs <- wolvesOnJuvs/(otherOnJuvs + wolvesOnJuvs)

# Adults
allOnAdults <- wolvesOnAdults/effectivenessAdults
otherOnAdults <- allOnAdults - wolvesOnAdults


a <- (otherOnAdults + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1]
#   In InitialEvent units --> (0.277 + Threat2_InitialFreq) / Threat2_InitialFreq
b <- (otherOnJuvs + Threat2_InitialFreq[1]) / Threat2_InitialFreq[1]
#   Wolf part In InitialEvent units --> ((0.0186 - 0.07375) + Threat1_InitialFreq) / Threat1_InitialFreq
d <- ((wolvesOnAdults) + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1]
#   Wolf part In InitialEvent units --> ((0.444584 - 0.3185) + Threat2_InitialFreq) / Threat2_InitialFreq
# Wolf part In InitialEvent units --> 
f <- ((wolvesOnJuvs) + Threat2_InitialFreq) / Threat2_InitialFreq

# This should be the effect on adults in initial units
Threat1_barrier_1 <- (a - 1)  + (d - 1)  + 1 # in initial units
# this should be the effect on Juvniles in initial units 
(b - 1)  + (f - 1)  + 1 # in initial units 
# Sum of compensatory causes and non-compensatory causes 
#  of "the predation effect", which is PropMortDay30ToSurvey_predation

# should be the same as equal:
sapply(Threat1_barriers, eval)[[1]] # :)
# also, lambda values should equal
#Threat1_LambdaEffect = wolvesOnAdults + otherOnAdults # check
#Threat2_LambdaEffect = wolvesOnAdults + otherOnJuvs # check

########################
# Step 6: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) vlaues
# these values can be changed to better reflect data as it becomes available:
### the first number of each function is the probability that the threat affects the citical event (lambda below zero)
### the subsequent numbers of each function are the probability that the barrers will FAIL to prevent the threat

# Top Event Frequency Function
# calculate the top event frequency, before mitigation actions take place.
topEventCalculator <- function(...) {
  sum(unlist(list(...)))
}

# Consequency Frequency Function
# Calculate lambda after mitigation actions take place
mitigate <- function(cull, pens, rs){
  cull * pens* rs
}

#######################################################
# step 7: sum the current events to get the total top event frequency
# Summarized in Table 2 of the manuscript (Winder et al. 2019)

threatCalculator <- function(init, barriers) {
  init * prod(barriers)
}


(topEvent <- topEventCalculator(threatCalculator(Threat1_InitialFreq, sapply(Threat1_barriers, eval)), 
                                threatCalculator(Threat2_InitialFreq, Threat2_barriers), 
                                threatCalculator(Threat3_InitialFreq, Threat3_barriers), 
                                threatCalculator(Threat4_InitialFreq, Threat4_barriers)))
# in other words, the sum of the Threat_topevents - which meets the LOPA users manual instructions
message("This is the top event lambda: ", round(((1 - topEvent) + 1), 3)) 

# does the top event exceed the lambda quartile?
print(TopEvent_lambda <- lambdaQuartile < 1 + (1-topEvent))

##################################################################################################
# step 8: what about after mitigation?
postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

postMitigateS <- postMitigate(topEvent, mitigate(0.814, 0.950, 0.950)) # combined mitigation/normal scenario
# values are from the literature, and can be changed # ASSUMTION/DATA
message("This is the consequence frequency: ", postMitigateS)

# Convert these inital frequency to values of lambda:

Mitigation_LambdaEffect <- list()
Mitigation_LambdaEffect[[1]] <- topEvent * (1 - 0.95) # linear restoration mitigation value, in Lambda units
Mitigation_LambdaEffect[[2]] <- topEvent* (1 - 0.814) # wolf cull mitigation value, in Lambda units
Mitigation_LambdaEffect[[3]] <- topEvent * (1 - 0.95) # maternal penning (exclosures) mitigation value, in Lambda units 


print(PostMitigate_lambda <- 1 + (1-topEvent) < 1 + (1-postMitigateS))
message("This is the consequence lambda: ", (1 + (1-postMitigateS)))

#######################
# Step 8b: look at different management scenarios by changing the alternate value to equal 1 (i.e. no effect)
# un-comment below lines to look at these strategies, and how the postmitigate value changes

#postMitigateS <- postMitigate(topEvent, mitigate(0.814, 1, 1)) # this is the wolfcull lever - acting solo
#postMitigateS <- postMitigate(topEvent, mitigate(1, 0.95, 1)) # this is the maternity pen lever - acting solo # 1.16
#postMitigateS <- postMitigate(topEvent, mitigate(1, 1, 0.95)) # this is seismic lines - acting solo
postMitigateS <- postMitigate(topEvent, mitigate(1, 0.95, 0.95)) # maternity penning and linear restoration

message("This is the consequence lambda after mitigation: ", (1 + (1-postMitigateS)))

#################################################################################################################################
# Step 9: Print the BRAT table, with values
# this should be the same as Figure 2 in the manuscipt

# Threat 1
print("This is Threat 1")
message("Threat 1, Initial Frequency: ", Threat1_InitialFreq)
message ("Threat 1, Current top event Frequency: ", Threat1_topEvent)

message ("Threat1, barrier1, frequency: ", Threat1_barrier_1)
message ("Threat1, barrier2, frequency: ", Threat1_barriers[2])
message ("Threat1, barrier3, frequency: ", Threat1_barriers[3])
message ("Threat1, barrier4, frequency: ", Threat1_barriers[4])
message ("Threat1, barrier5, frequency: ", Threat1_barriers[5])
message ("Threat1, barrier6, frequency: ", Threat1_barriers[6])
message ("Threat1, barrier7, frequency: ", Threat1_barriers[7])

message ("Threat1, barriers, lambda list 1 through 7: ", Threat_LambdaEffect[c(1)]) # this goes in the comment box

message ("Threat 1, barrier1, comments, additive predation: ", wolvesOnAdults) # this goes in the comment box
message ("Threat 1, barrier1, comments, compensatory predation: ", otherOnAdults) # this goes in the comment box
message ("Threat 1, barrier1, comments, effectiveness on adults: ",  effectivenessAdults) # this goes in the comment box


# Threat 2
print("This is Threat 2")
message("Threat 2, Initial Frequency: ", Threat2_InitialFreq)
message ("Threat 2, Current top event Frequency: ", Threat2_topevent)

message ("Threat2, barrier2, frequency: ", Threat2_barrier_1)

message ("Threat2, barriers, lambda: ", Threat_LambdaEffect[c(2)]) # this goes in the comment box

message ("Threat 2, barrier2, comments, additive predation: ", wolvesOnJuvs) # this goes in the comment box
message ("Threat 2, barrier2, comments, compensatory predation: ", otherOnJuvs) # this goes in the comment box
message ("Threat 2, barrier2, comments, effectiveness on juveniles: ",  effectivenessJuvs) # this goes in the comment box

#Threat3
print ("this is Threat 3")
message("Threat 3, Initial Frequency: ", Threat3_InitialFreq)
message ("Threat 3, Current top event Frequency: ", Threat3_topevent)

message ("Threat3, barrier1, frequency: ", Threat3_barriers[1])
message ("Threat3, barrier2, frequency: ", Threat3_barriers[2])
message ("Threat3, barrier3, frequency: ", Threat3_barriers[3])

message ("Threat1, barriers, lambda list 1 through 3: ", Threat_LambdaEffect[c(3)]) # these go in the comment boxes

#Threat4
print ("this is Threat 4")
message("Threat 4, Initial Frequency: ", Threat4_InitialFreq)
message ("Threat 4, Current top event Frequency: ", Threat4_topevent)

message ("Threat4, barrier1, frequency: ", Threat4_barriers[1])
message ("Threat4, barrier2, frequency: ", Threat4_barriers[2])
message ("Threat4, barrier3, frequency: ", Threat4_barriers[3])
message ("Threat4, barrier3, frequency: ", Threat4_barriers[4])
message ("Threat4, barrier3, frequency: ", Threat4_barriers[5])

message ("Threat1, barriers, lambda list 1 through 5: ", Threat_LambdaEffect[c(4)]) # these go in the comment boxes


## Hazzard (Policy objective)
message ("This is the target frequency: ", (1+(1-lambdaQuartile)))
message ("This is the target lambda: ", lambdaQuartile) # put this in the risk event red circle
message ("This is the Current Total top event frequency: ", topEvent)
message ("This is the current Total top event lambda: ", (1+(1-topEvent))) # put this in comments


### Mitigation Boxes
message ("This is the 'responsive restoration of linear features to reduce access': ", 0.95)
message ("This is the 'responsive restoriation of linear features to reduce access': ", Mitigation_LambdaEffect[1]) # put this in the comment box

message ("This is the 'wolf cull': ", 0.814)
message ("This is the 'wolf cull': ", Mitigation_LambdaEffect[2]) # put this in the comment box

message ("This is the 'Intensive in situ conservation': ", 0.95)
message ("This is the 'Intensive in situ conservation': ", Mitigation_LambdaEffect[3]) # put this in the comment box

#### Consequency Box
message ("This is the acceptable consequency frequency: ", (1+(1-lambdaQuartile)))
message ("This is the current consequence frequency: ", postMitigateS)
message ("This is the current consequency lambda: ", (1+(1-postMitigateS))) # put this in a comment box


#################################################################################################################################
#################################################################################################################################

# Step 10: Repeat the above, but substitute in different demographic data for different herds
# In winder et al. 2019 we have repeated the above with two different herds:


# snake-sahtahneh herd (STUDY AREA 3) 
N = 360 # population of the Chinchaga herd as of 2008
SadF = 0.94 # Adult female survival 
recr = 0.072/2 # Juvenile female recruitment 
sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
#Also, set:
#wolfCullPropOnAdults <- MortSadF*0.8
#Threat1_InitialFreq <- 0.05549*0.9 # ASSUMPTION

# and An "averaged" population
# ASSUMPTION: No one herd demography is accurate for a study area. Instead, we quantify the BRAT analysis at the meta-region.
N = (360 +  250)/2 # population of the Chinchaga herd as of 2008
SadF = (0.94 + 0.87)/2 # Adult female survival 
recr = ((0.072 + 0.139)/2)/2 # Juvenile female recruitment 
sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

##################################################################################################################################
# the results of this work is summarized in Tables 2 and 4 of the current BRAT manuscript (Winder et al. 2019)
##################################################################################################################################

###################################################################################################################################
# Other information:
###################################################################################################################################
# #################################################################################################################################
# # Threat 1 initial Frequency calcultion laid out
# 
# Threat2_barrier_1
# 
# TEF = prod(Initial Frequency * barriers)
# 0.054 = prod (wolves on adults + other on adults)*1.2285
# 
# wolvesOnAdults
# other on adults = x
# 
# wolvesOnAdults
# wolvesOnJuvs
# 1-wolfCullBarrier
# 
# For juveniles we know that:
# TEF = prod(initial Frequency * barriers)
# 
# effectiveness of wolves on juveniles
# ## assume this ratio (30.7% effectiveness) applies to adults, AND to other populations
# 
# total juvienile predation
# = other on juvs + wolves on juvs (all in lambda units)
# 
# threatBarrier1 = (total juvenile predation/inital frequency) + 1
