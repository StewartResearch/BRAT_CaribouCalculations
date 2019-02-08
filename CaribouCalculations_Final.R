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
Threat1_barriers <- c(quote(Threat1_barrier_1), 1.40, 0.65, 1.5, 1.0, 0.9, 1.0) # in units of Initial Frequency
Threat2_barriers <- quote(c(Threat2_barrier_1)) # in units of Initial Frequency # This is derived from data below
Threat3_barriers <- c(1.05, 1, 1) # in units of Initial Frequency
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
recr = 0.13 # Juvenile recruitment 
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
Threat2_multiplier = Mortrecr * 0.95 # ASSUMPTION # 5% of random juvenile deaths are due to factors other than predation (calculated below based on maternity pen literature and pers coms)
Threat3_multiplier = MortSadF * 0.05 + Mortrecr * 0.025 # ASSUMPTION # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
Threat4_multiplier = Threat3_multiplier # ASSUMPTION

# Assumptions about caribou population demongraphics, from published literaure, reports, and pers. comms:
# These values are used to calculate the current top event frequency, for juveniles
# pregnancy rate
pregR <- 0.9 # DATA/ASSUMPTION # pregnancy rate - from observations in maternal pens 
# (Scott McNay pers com, Kinse-Za maternal penning project)
# juvenile survival to the fist day of life
surv1stDay <- 0.8 # DATA/ASSUMPTION # juvenile survival to the first day - from maternal pen observations 
# (Scott McNay pers com, Klinse-Za maternal penning project)
mort1stDay <- 1 - surv1stDay 

####################################################################################
# # Step 3: first calculate the population growth distribution - non density dependent

# We dont need this for the BRAT framework, but it is neat to see:

# annual <- function(N, SadF, recr) { 
#   newN <- N * SadF
#   newN <- newN + newN * (recr / 2)
#   round(newN, 0)
# }
# 
# Nyears <- 100 
# N <- integer(Nyears)
# N[1] <- 305
# for (yr in 1:Nyears) {
#   N[yr + 1] <- annual(N[yr], SadF, recr)
# }
# # print(N) # population size at each year, for 100 years.

###########################################################################################################
# Step 4: calculate a normal distribution, and indicate the 60% value
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
# Step 5:populating the BRAT framework Initial frequency and Current top events of threats
# Step 5 thorugh 7 are summarized in Table 4 of the manuscript (Winder et al. 2019)

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
Threat1_InitialFreq <- 0.05549 # ASSUMPTION
# This calculation involved back calculating from the wolf mitigation initial frequency, to the juvenile predation threat frequency,
## translating this value into lambda units of wolf predation and compensatory predation, calculating the ratio between these
## predation effects, and applying the ratio to adult wolf vs compensatory predation. From there we could substitute in the current
## top event frequency (known from ECCC data), to get this inital frequency value.
# ASSUMPTION: the effectiveness of wolfculls extends from juveniles to adults equally
# ASSUMPTION: This is a constant applied to all populations to determine the threat values of adult predation. This might be a big assumption!

Threat1_barrier_1 <-  Threat1_multiplier / (Threat1_InitialFreq * prod(unlist(Threat1_barriers[-1])))
Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality
Threat1_InitialFreq <- Threat1_topEvent/prod(sapply(Threat1_barriers, eval))

### Initial Frequency
##### calculated from the BRAT threat line:
predationAdultRev <- function(Threat1_InitialFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}


# Threat 2 - Juvenile predation
### Initial Frequency
##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is a factor)
# Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
Threat2_InitialSurv <- pregR * SadF * surv1stDay # proportion of females that produce a calf
Threat2_InitialFreq <- (1 - (Threat2_InitialSurv)) # proportion of females at the start of a year that result in no calf a year later

### Current Top Event Frequency
##### Difference between the calculated Initial Frequency, and the ECCC reported value (i.e. how 
#  many calf deaths from day 1 to a population survey are unaccounted for?)
PropMortDay30ToSurvey = Mortrecr - Threat2_InitialFreq 
# We know from maternal penning data that even between day 2 and day 30, 90% of calves survive 
#   (i.e. even without predation, 90% of calves survive, and 10% die from "other causes; Scott McNay pers comm - Klinse-Za meternity pen)
## Assume this rate remains consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
PropMortDay30ToSurvey_predation <- PropMortDay30ToSurvey * 0.9 # DATA/ASSUMPTION #
# Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
Threat2_topevent <- PropMortDay30ToSurvey_predation + Threat2_InitialFreq

### Barrier value: Effect of predator avoidance
##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
Threat2_barriers <- Threat2_topevent / Threat2_InitialFreq
Threat2_barrier_1 <- Threat2_barriers[1]


# Needed for the rest of the framework:
### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
Difference_nonpredation = PropMortDay30ToSurvey - PropMortDay30ToSurvey_predation
### Therefore, we need to add the multiplier of 1/2 this rate to the final two Threats, to account for the proportion 
# directly attributable to juveniles
### Remember at the start of this script I already specified Threat multipliers. You can change them there if needed.

# Threat 3: Permanent habitat appropriation
### Current Top Event Frequency
Threat3_topevent = Threat3_multiplier + 0.5 * Difference_nonpredation # DATA/ASSUMPTION

### Initial Frequency
# multiply it by what values exist in the barrier to backcalculte this value
Threat3_InitialFreq <- Threat3_topevent/(prod(Threat3_barriers))


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier + 0.5 * Difference_nonpredation # DATA/ASSUMPTION 

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
# Threat4_InitialFreq <- Threat4_multiplier*(prod(0.95, 0.950, 0.95, 1.00, 1.00))
Threat4_InitialFreq <- Threat4_topevent/(prod(Threat4_barriers))

#######################################################
# step 6: Convert the inital frequency values to values of lambda:

Threat_LambdaEffect <- list()
Threat_LambdaEffect[[1]] <- Threat1_InitialFreq * sapply(Threat1_barriers, eval) - Threat1_InitialFreq # in Lambda units
Threat_LambdaEffect[[2]] <- Threat2_InitialFreq * Threat2_barriers - Threat2_InitialFreq # in Lambda units
Threat_LambdaEffect[[3]] <- Threat3_InitialFreq * Threat3_barriers - Threat3_InitialFreq # in Lambda units
Threat_LambdaEffect[[4]] <- Threat4_InitialFreq * Threat4_barriers - Threat4_InitialFreq # in Lambda units

wolfCullEffect <- 0.186 # DATA/ASSUMPTION 
# i.e. lambda has the potential to increase by 18.6% during a full wolf cull.
# Hervieux et al. (2014) found lambda increased between 4.6 (within herds) and 14% (between herds) during a 50% wolf cull. 
# We avereaged these fundings, and standardized by 50% wolf cull to get the 18.6%
# this assumes a linear relationship

#############################################################################################################################
# Step 6b
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
wolvesOnAdults/(otherOnAdults + wolvesOnAdults)


#  "other predation" for calves = 0.444584 - 0.9 * wolfCullEffect = 0.277 in Lambda units
#     i.e. Threat_Lambda[[2]] - (0.1*wolfcullEffect)
#  "other predation" for calves = 
wolvesOnJuvs <- wolfCullPropOnJuvs * wolfCullEffect # Lambda units
otherOnJuvs <- Threat_LambdaEffect[[2]][1] - wolvesOnJuvs # Lambda units

# Percent effectiveness on juvenile recruitment:
effectiveness <- wolvesOnJuvs/(otherOnJuvs + wolvesOnJuvs)

# Adults
allOnAdults <- wolvesOnAdults/effectiveness
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
# Step 7: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) vlaues
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
# step 8: sum the current events to get the total top event frequency
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

# what about after mitigation?
postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

postMitigate <- postMitigate(topEvent, mitigate(0.814, 0.950, 0.95)) # combined mitigation/normal scenario
print(PostMitigate_lambda <- 1 + (1-topEvent) < 1 + (1-postMitigate))

#######################
# Step 8b: look at different management scenarios by changing the alternate value to equal 1 (i.e. no effect)
# un comment below lines to look at these strategies, and how the postmitigate value changes

#postMitigate <- postMitigate(topEvent, mitigate(1, 0.95, 1)) # this is the maternity pen lever - acting solo # 1.16
#postMitigate <- postMitigate(topEvent, mitigate(0.814, 1, 1)) # this is the wolfcull lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 0.95)) # this is seismic lines - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(1, 0.95, 0.95)) # maternity penning and linear restoration

print(PostMitigate_lambda <- 1 + (1-topEvent) < 1 + (1-postMitigate))

#################################################################################################################################
#################################################################################################################################


#################################################################################################################################
# Repeat the above, but substitute in different demographic data for different herds
# In winder et al. 2019 we have repeated the above with two different herds:


# snake-sahtahneh herd (STUDY AREA 3) 
N = 360 # population of the Chinchaga herd as of 2008
SadF = 0.94 # Adult female survival 
recr = 0.072 # Juvenile recruitment 
sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

# and An "averaged" population
# ASSUMPTION: No one herd demography is accurate for a study area. Instead, we quantify the BRAT analysis at the meta region.
N = (360 +  250)/2 # population of the Chinchaga herd as of 2008
SadF = (0.94 + 0.87)/2 # Adult female survival 
recr = (0.072 + 0.139)/2 # Juvenile recruitment 
sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

# the results of this work is summarized in Tables 2 and 4 of the current BRAT manuscript (Winder et al. 2019)
