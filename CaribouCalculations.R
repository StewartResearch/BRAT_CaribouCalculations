#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# January 2019
# By Eliot McIntire and Frances Stewart
#################################################################################

# Objectives: ----
### Calculate target frequency for the critical event
##### Policy says the target frequency should be 40% (Environment and Climate Change Canada 2012 Recovery Strategy) - ECCC has indicated landscape change should
##### not exceed 35%, as this produces a 60% probability of herd persistence. In otehr words, this policy threshold is stated at 40% risk of population decline
##### for any specific population. Hence for 40%
####### But this 40% is obtained from a normal distribution - i.e. it is the area under a normalized curve (bc the models in ECCC2012 assume a normal distribution)
####### representing the lower 40% of the distribution.

# See Calculations_global.R for everything being developped prior to this stage.

###########################
###########################
# Chinchaga herd (STUDY AREA 1) ---- 
###########################
# measuring lambda and associated thresholds for the Hazards: Part I ----

# Step 1: know (or estimate) some basic information about each study area/herd. 
### For Chinchaga: data obtained from the ECCC 2011 scientific report
N = 250 # population of the Chinchaga herd as of 2008
SadF = 0.87 # Adult female survival 
recr = 0.13 # Juvenile recruitment 

# invert these numbers to calculate the mortality values:
MortSadF = 0.13 # part of this will go into the Initial frequency of the threats
Mortrecr = 0.87 # part of this will go into the Initial frequency of the threats


# Step 2: State your assumptions
### In this example, there are 4 threats. Three of these threats apply to adult females, and three of these threats apply to juveniles
### one threat each, applies to only adult females and juveniles
##### We need to separate out a potion of the above mortality measures for each of the frequencies (whether initial, or current top event) for each threat.
####### current top event frequencies are from known data (ie. known mortality or recruitment)
####### initial event frequencies are things that we need to find out - ie. what was the frequency of this threat if there had been no landscape changes
##### The literature says that most of the mortality is due to predation. 
#####

Threat1_multiplier = MortSadF*0.90 # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
Threat2_multiplier = Mortrecr*0.95 # 5% of random juvenile deaths are due to factors other than predation (calculated below based on maternity pen literature and pers coms)
Threat3_multiplier = MortSadF*0.05 + 0.025 # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
Threat4_multiplier = Threat3_multiplier

# assumptions about caribou population demongraphics, from pblished literaure, reports, and pers. comms.
pregR <- 0.9 # pregnancy rate - from observations in maternal pens (Scott McNay pers com)
surv1stDay <- 0.8 # juvenile survival to the first day - from maternal pen observations (Scott McNay pers com)
mort1stDay <- 1 - surv1stDay 
#survToSurvey <- recr / prod(SadF, pregR, surv1stDay) 
#mortToSurvey <- 1 - survToSurvey


# Step 3: first calculate the population growth distribution - non density dependent
annual <- function(N, SadF, recr) { 
  newN <- N * SadF
  newN <- newN + newN * (recr / 2)
  round(newN, 0)
}

Nyears <- 100 
N <- integer(Nyears)
N[1] <- 250
for (yr in 1:Nyears) {
  N[yr + 1] <- annual(N[yr], SadF, recr)
}
print(N) # population size at each year, for 100 years.


# Step 4: calculate a normal distribution, and indicate the 40% value
### This 40% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
### In other words, we are OK with 40% of the herds dissapearing, on average.

sumMort <- function(SadF, recr) { 
  1 - SadF + (1 - (recr / 2)) # only 1/2 he calves are female, and will contribute to the next population!
}

sd1 <- 0.3 # this value can go as high as 0.3, from the literature.
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd of sd1
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 


# Step 4: put this mortQuantile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print( (1 - mortQuantile) + 1)
# 1.07

threatCalculator <- function(init, barriers) {
  init * prod(barriers)
}
# Step 5: then calculate how the existing threats, and barriers to those threats, affect this quartile:
### Threat 1
predationAdult <- function(init, barriers) { #avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) {
  init * prod(barriers) #avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey
}

predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}

### Threat 2
predationJuv <- function(init, barriers) { #avoid) {
  init * prod(barriers) #avoid
}

predationJuvRev <- function(TopEventFreq, avoid) {
  TopEventFreq/( avoid )
}

### Threat 3
habitatAppropriation <- function(init, barrierssetAsides, recoveryRestoration, afforestation) {
  init * setAsides * recoveryRestoration * afforestation
}

### Threat 4
stress <- function(init, managementForFood, dailySelection, stableEpidemiology, resistance, preventHarassment) {
  init * managementForFood * dailySelection * stableEpidemiology * resistance * preventHarassment
}

#########################################################################################################################################################
#########################################################################################################################################################
# Rational for populating the BRAT framework
#########################################################################################################################################################

# Determining what claues should go into the Initial Frequency of a Threat (Blue boxes), and what values should go into the barriers to a threat (White
# boxes) can be tricky and subjective in the BRAT framework. The values need to come from the best sourced data, and when those data are limited, they
# can be hard to determine. However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
# available. Our manuscript here is show how, and can be changed as new information comes to light - not only the number of threats, barriers to a threat,
# can be changed, but the associations between these items can be changed too.


# Rationale for calculated threat vlaues
# These thoughts are also documented in 'Quantification questions_FScomments.docx'


# Threat 1 - General predation
### Current Top Event Frequency
Threat1_InitialFreq <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality

### Initial Frequency
##### calculated from the BRAT threat line:
predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}
# Threat1_topEvent <- predationAdult(Threat1_InitialFreq, 2, 1.5, 0.65, 1.1, 1, 0.9)


# Threat 2 - juvenile predation
### Initial Frequency
##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is not a factor)
# Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
Threat2_InitialSurv<- pregR*SadF*surv1stDay # proportion of females that produce a calf
Threat2_InitialFreq<- (1 - (Threat2_InitialSurv)) # proportion of females that loose a calf

### Current Top Event Frequency
##### Difference between the calculated Initial Frequency, and the ECCC reported value (i.e. how many calf deaths from day 1 to a population survey are unaccounted for?)
Difference = Mortrecr - Threat2_InitialFreq 
# We know from maternal penning data that even between day 2 and day 30, only 90% of calves survive (i.e. even without predation, 90% of calves survive, and 10% die from "other causes; McNay work)
## Assume this rate remains consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
Difference_predation = Difference*0.9 
# Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
Threat2_topevent <- Difference_predation + Threat2_InitialFreq

### Barrier value: Effect of predator avoidance
##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
Threat2_barrier <- Threat2_topevent/Threat2_InitialFreq


# Needed for the rest of the framework:
### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
Difference_nonpredation = Difference - Difference_predation
### Therefore, we need to add the multiplier of 1/2 this rate to the final two Threats, to account for the proportion directly attributable to juveniles
### Remember at the start of this script I already specified Threat multipliers. You can change them there if needed.
Threat3_multiplier
Threat4_multiplier


# Threat 3: Permanent habitat appropriation
### Current Top Event Frequency
Threat3_topevent = Threat3_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat3_InitialFreq <- Threat3_multiplier*(prod(0.95* 1* 1))


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat4_InitialFreq <- Threat4_multiplier*(prod(1, 0.8, 1, 1, 1))
########################################################################################################################################################


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
mitigate <- function(cull, dd, pens){
  cull * dd * pens
}


###
#topEvent <- topEvent(predationAdult(Threat1_InitialFreq, 2, 1.5, 0.65, 1.1, 1, 0.9), # combined mitigation/normal scenario
#                    predationJuv(Threat2_InitialFreq, 2.19), 
#                   habitatAppropriation(Threat3_InitialFreq, 0.95, 1, 1), 
#                  stress(Threat4_InitialFreq, 1, 0.8, 1, 1, 1))

Threat1_barriers <- c(1.75, 1.5, 0.65, 1.05, 1, 0.9, 1) # in units of Initial Frequency
Threat2_barriers <- c(2.19) # in units of Initial Frequency
Threat3_barriers <- c(0.95, 1, 1) # in units of Initial Frequency
Threat4_barriers <- c(1.5, 0.8, 1.25, 1.25, 1)  # in units of Initial Frequency


(topEvent <- topEventCalculator(threatCalculator(Threat1_InitialFreq, Threat1_barriers), # climate change scenario
                               threatCalculator(Threat2_InitialFreq, Threat2_barriers), 
                               threatCalculator(Threat3_InitialFreq, Threat3_barriers), 
                               threatCalculator(Threat4_InitialFreq, Threat4_barriers)))
message("This is the top event lambda: ", round(1 + (1 - topEvent), 2)) # this should be around 0.93 (ECCC 2008), but slightly lower as we know that
# Chinchaga herd is declining (ie, below our threshold of 0.93 for 40% probability of stability)
###

postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 0.95)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(0.5, 1, 1)) # this is wolf cull lever - acting solo.
postMitigate <- postMitigate(topEvent, mitigate(0.85, 0.95, 1)) # combined mitigation/normal scenario
#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # Climate change scenario

# Step 7: look at the hazard and consequence values, and compare
message("Lambdas: ")
print(mortQuantile) # this should be the Hazard target frequency
print( (1 - topEvent) + 1) # this should be the hazard current total top event frequency
print( (1 - postMitigate) + 1) # this is the current consequence frequency

#message("Lambdas: ")
#print(mortQuantile)  # this should be the Hazard target frequency
#topEventN <- ((1 - topEvent) 
#print(topEventN)  # this should be the hazard current total top event frequency
#postMitigateN <- (1 - postMitigate)
#print(postMitigateN) # this is the current consequence frequency


# Print associated lopa crit messages by comparing these values:
out1 <- ifelse(topEvent >= mortQuantile, "Red - caribou population growth rate is persistently below 1", "Green - caribou population growthrate is above 1")
out2 <- ifelse(postMitigate >= mortQuantile, "Red - caribou extirpated", "Green - caribou recovered")
print(out1)
print(out2)
# Your solution!

############################################
# In Lambda Units
############################################
Threat_LambdaEffect <- list()
Threat_LambdaEffect[[1]] <- Threat1_InitialFreq * Threat1_barriers - Threat1_InitialFreq # in Lambda units
Threat_LambdaEffect[[2]] <- Threat2_InitialFreq * Threat2_barriers - Threat2_InitialFreq # in Lambda units
Threat_LambdaEffect[[3]] <- Threat3_InitialFreq * Threat3_barriers - Threat3_InitialFreq # in Lambda units
Threat_LambdaEffect[[4]] <- Threat4_InitialFreq * Threat4_barriers - Threat4_InitialFreq # in Lambda units

wolfCullEffect <- 0.14
#  So, 0.444 is predator effect on calves -- partition into "non-compensated wolf" == "True effect of wolves on calves" 
#     and "other" e.g., 0.14 is max possible... partition amongst adults and juvs e.g., 0.9 and 0.1 -->
#     0.9 * 0.14 on calves and 0.1 * 0.14  on adults
#  "other predation" for adults = 0.08775 - 0.1 * wolfCullEffect = 0.07375 in Lambda units
#  "other predation" for adults = 
wolvesOnAdults <- 0.1 * wolfCullEffect # =  0.07375 in Lambda units
otherOnAdults <- Threat_LambdaEffect[[1]][1] - wolvesOnAdults # =  0.07375 in Lambda units
#  "other predation" for calves = 0.444584 - 0.9 * wolfCullEffect = 0.3185 in Lambda units
#  "other predation" for calves = 
wolvesOnJuvs <- 0.9 * wolfCullEffect
otherOnJuvs <- Threat_LambdaEffect[[2]][1] - wolvesOnJuvs # = 0.3185 in Lambda units


#   In InitialEvent units --> (0.07375 + Threat1_InitialFreq) / Threat1_InitialFreq
a <- (otherOnAdults + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1]
#   In InitialEvent units --> (0.3185 + Threat2_InitialFreq) / Threat2_InitialFreq
b <- (otherOnJuvs + Threat2_InitialFreq[1]) / Threat2_InitialFreq[1]
#   Wolf part In InitialEvent units --> ((0.08775 - 0.07375) + Threat1_InitialFreq) / Threat1_InitialFreq
d <- ((wolvesOnAdults) + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1]
#   Wolf part In InitialEvent units --> ((0.444584 - 0.3185) + Threat2_InitialFreq) / Threat2_InitialFreq
# Wolf part In InitialEvent units --> 
f <- ((wolvesOnJuvs) + Threat2_InitialFreq) / Threat2_InitialFreq

wolfCullEffect * 0.1

(a - 1)  + (d - 1)  + 1



###########################
###########################
# Maxhamish herd (STUDY AREA 2) ---- 
###########################
# measuring lambda and associated thresholds for the Hazards: Part I ----

# Step 1: know (or estimate) some basic information about each study area/herd. 
### Made up datat - that is roughly reflected of expected rec and sadF on a population that is larger than the other two study areas.
N = 600 # 
SadF = 0.85 # Adult female survival 
recr = 0.30 # Juvenile recruitment 

# invert these numbers to calculate the mortality values:
MortSadF = 0.15 # part of this will go into the Initial frequency of the threats
Mortrecr = 0.70 # part of this will go into the Initial frequency of the threats


# Step 2: State your assumptions
### In this example, there are 4 threats. Three of these threats apply to adult females, and three of these threats apply to juveniles
### one threat each, applies to only adult females and juveniles
##### We need to separate out a potion of the above mortality measures for each of the frequencies (whether initial, or current top event) for each threat.
####### current top event frequencies are from known data (ie. known mortality or recruitment)
####### initial event frequencies are things that we need to find out - ie. what was the frequency of this threat if there had been no landscape changes
##### The literature says that most of the mortality is due to predation. 
#####

Threat1_multiplier = MortSadF*0.90 # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
Threat2_multiplier = Mortrecr*0.95 # 5% of random juvenile deaths are due to factors other than predation (calculated below based on maternity pen literature and pers coms)
Threat3_multiplier = MortSadF*0.05 + 0.025 # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
Threat4_multiplier = Threat3_multiplier

# assumptions about caribou population demongraphics, from pblished literaure, reports, and pers. comms.
pregR <- 0.9 # pregnancy rate - from observations in maternal pens (Scott McNay pers com)
surv1stDay <- 0.8 # juvenile survival to the first day - from maternal pen observations (Scott McNay pers com)
mort1stDay <- 1 - surv1stDay 
#survToSurvey <- recr / prod(SadF, pregR, surv1stDay) 
#mortToSurvey <- 1 - survToSurvey


# Step 3: first calculate the population growth distribution - non density dependent
annual <- function(N, SadF, recr) { 
  newN <- N * SadF
  newN <- newN + newN * (recr / 2)
  round(newN, 0)
}

Nyears <- 100 
N <- integer(Nyears)
N[1] <- 250
for (yr in 1:Nyears) {
  N[yr + 1] <- annual(N[yr], SadF, recr)
}
print(N) # population size at each year, for 100 years.


# Step 4: calculate a normal distribution, and indicate the 40% value
### This 40% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
### In other words, we are OK with 40% of the herds dissapearing, on average.

sumMort <- function(SadF, recr) { 
  1 - SadF + (1 - (recr / 2)) # only 1/2 he calves are female, and will contribute to the next population!
}

sd1 <- 0.3 # this value can go as high as 0.3, from the literature.
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd of sd1
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 


# Step 4: put this mortQuantile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print( (1 - mortQuantile) + 1)
# 1.02

# Step 5: then calculate how the existing threats, and barriers to those threats, affect this quartile:
### Threat 1
predationAdult <- function(init, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) {
  init * avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey
}

predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}

### Threat 2
predationJuv <- function(init, avoid) {
  init * avoid
}

predationJuvRev <- function(TopEventFreq, avoid) {
  TopEventFreq/( avoid )
}

### Threat 3
habitatAppropriation <- function(init, setAsides, recoveryRestoration, afforestation) {
  init * setAsides * recoveryRestoration * afforestation
}

### Threat 4
stress <- function(init, managementForFood, dailySelection, stableEpidemiology, resistance, preventHarassment) {
  init * managementForFood * dailySelection * stableEpidemiology * resistance * preventHarassment
}

#########################################################################################################################################################
#########################################################################################################################################################
# Rational for populating the BRAT framework
#########################################################################################################################################################

# Determining what claues should go into the Initial Frequency of a Threat (Blue boxes), and what values should go into the barriers to a threat (White
# boxes) can be tricky and subjective in the BRAT framework. The values need to come from the best sourced data, and when those data are limited, they
# can be hard to determine. However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
# available. Our manuscript here is show how, and can be changed as new information comes to light - not only the number of threats, barriers to a threat,
# can be changed, but the associations between these items can be changed too.


# Rationale for calculated threat vlaues
# These thoughts are also documented in 'Quantification questions_FScomments.docx'


# Threat 1 - General predation
### Current Top Event Frequency
Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality

### Initial Frequency
##### calculated from the BRAT threat line:
predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}
Threat1_InitialFreq<- predationAdult(Threat1_topEvent, 2, 1.5, 0.65, 1.1, 1, 0.9)


# Threat 2 - juvenile predation
### Initial Frequency
##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is not a factor)
# Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
Threat2_InitialSurv<- pregR*SadF*surv1stDay # proportion of females that produce a calf
Threat2_InitialFreq<- (1 - (Threat2_InitialSurv)) # proportion of females that loose a calf

### Current Top Event Frequency
##### Difference between the calculated Initial Frequency, and the ECCC reported value (i.e. how many calf deaths from day 1 to a population survey are unaccounted for?)
Difference = Mortrecr - Threat2_InitialFreq 
# We know from maternal penning data that even between day 2 and day 30, only 90% of calves survive (i.e. even without predation, 90% of calves survive, and 10% die from "other causes; McNay work)
## Assume this rate remains consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
Difference_predation = Difference*0.9 
# Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
Threat2_topevent <- Difference_predation + Threat2_InitialFreq

### Barrier value: Effect of predator avoidance
##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
Threat2_barrier <- Threat2_topevent/Threat2_InitialFreq


# Needed for the rest of the framework:
### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
Difference_nonpredation = Difference - Difference_predation
### Therefore, we need to add the multiplier of 1/2 this rate to the final two Threats, to account for the proportion directly attributable to juveniles
### Remember at the start of this script I already specified Threat multipliers. You can change them there if needed.
Threat3_multiplier
Threat4_multiplier


# Threat 3: Permanent habitat appropriation
### Current Top Event Frequency
Threat3_topevent = Threat3_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat3_InitialFreq <- Threat3_multiplier*(prod(0.95* 1* 1))


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat4_InitialFreq <- Threat4_multiplier*(prod(1, 0.8, 1, 1, 1))
########################################################################################################################################################


# Step 6: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) vlaues
# these values can be changed to better reflect data as it becomes available:
### the first number of each function is the probability that the threat affects the citical event (lambda below zero)
### the subsequent numbers of each function are the probability that the barrers will FAIL to prevent the threat

# Top Event Frequency Function
# calculate the top event frequency, before mitigation actions take place.
topEvent <- function(...) {
  sum(unlist(list(...)))
}

# Consequency Frequency Function
# Calculate lambda after mitigation actions take place
mitigate <- function(cull, dd, pens){
  cull * dd * pens
}


topEvent <- topEvent(predationAdult(Threat1_InitialFreq, 2, 1.5, 0.65, 2, 1, 0.9), # climate change scenario
                     predationJuv(Threat2_InitialFreq, 2.19), 
                     habitatAppropriation(Threat3_InitialFreq, 0.95, 2, 1), 
                     stress(Threat4_InitialFreq, 2, 2, 2, 1, 1))
###

postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 0.95)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(0.5, 1, 1)) # this is wolf cull lever - acting solo.
#postMitigate <- postMitigate(topEvent, mitigate(0.5, 1, 0.95)) # combined mitigation/normal scenario
postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # Climate change scenario

# Step 7: look at the hazard and consequence values, and compare
message("Lambdas: ")
print( (1 - mortQuantile) + 1) # this should be the Hazard target frequency
print( (1 - topEvent) + 1) # this should be the hazard current total top event frequency
print( (1 - postMitigate) + 1) # this is the current consequence frequency

#message("Lambdas: ")
#print(mortQuantile)  # this should be the Hazard target frequency
#topEventN <- ((1 - topEvent) 
#print(topEventN)  # this should be the hazard current total top event frequency
#postMitigateN <- (1 - postMitigate)
#print(postMitigateN) # this is the current consequence frequency


# Print associated lopa crit messages by comparing these values:
out1 <- ifelse(topEvent >= mortQuantile, "Red - caribou population growthrate is persistently below 1", "Green - caribou population growthrate is above 1")
out2 <- ifelse(postMitigate >= mortQuantile, "Red - caribou extirpated", "Green - caribou recovered")
print(out1)
print(out2)
# Your solution!



###########################
###########################
# Snake- Sahtahneh herd (STUDY AREA 3) ---- 
###########################
# measuring lambda and associated thresholds for the Hazards: Part I ----

# Step 1: know (or estimate) some basic information about each study area/herd. 
### Made up datat - that is roughly reflected of expected rec and sadF on a population that is larger than the other two study areas.
N = 360 # 
SadF = 0.94 # Adult female survival 
recr = 0.072 # Juvenile recruitment 

# invert these numbers to calculate the mortality values:
MortSadF = 0.06 # part of this will go into the Initial frequency of the threats
Mortrecr = 0.928 # part of this will go into the Initial frequency of the threats


# Step 2: State your assumptions
### In this example, there are 4 threats. Three of these threats apply to adult females, and three of these threats apply to juveniles
### one threat each, applies to only adult females and juveniles
##### We need to separate out a potion of the above mortality measures for each of the frequencies (whether initial, or current top event) for each threat.
####### current top event frequencies are from known data (ie. known mortality or recruitment)
####### initial event frequencies are things that we need to find out - ie. what was the frequency of this threat if there had been no landscape changes
##### The literature says that most of the mortality is due to predation. 
#####

Threat1_multiplier = MortSadF*0.90 # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
Threat2_multiplier = Mortrecr*0.95 # 5% of random juvenile deaths are due to factors other than predation (calculated below based on maternity pen literature and pers coms)
Threat3_multiplier = MortSadF*0.05 + 0.025 # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
Threat4_multiplier = Threat3_multiplier

# assumptions about caribou population demongraphics, from pblished literaure, reports, and pers. comms.
pregR <- 0.9 # pregnancy rate - from observations in maternal pens (Scott McNay pers com)
surv1stDay <- 0.8 # juvenile survival to the first day - from maternal pen observations (Scott McNay pers com)
mort1stDay <- 1 - surv1stDay 
#survToSurvey <- recr / prod(SadF, pregR, surv1stDay) 
#mortToSurvey <- 1 - survToSurvey


# Step 3: first calculate the population growth distribution - non density dependent
annual <- function(N, SadF, recr) { 
  newN <- N * SadF
  newN <- newN + newN * (recr / 2)
  round(newN, 0)
}

Nyears <- 100 
N <- integer(Nyears)
N[1] <- 250
for (yr in 1:Nyears) {
  N[yr + 1] <- annual(N[yr], SadF, recr)
}
print(N) # population size at each year, for 100 years.


# Step 4: calculate a normal distribution, and indicate the 40% value
### This 40% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
### In other words, we are OK with 40% of the herds dissapearing, on average.

sumMort <- function(SadF, recr) { 
  1 - SadF + (1 - (recr / 2)) # only 1/2 he calves are female, and will contribute to the next population!
}

sd1 <- 0.3 # this value can go as high as 0.3, from the literature.
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd of sd1
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 


# Step 4: put this mortQuantile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print( (1 - mortQuantile) + 1)
# 1.02

# Step 5: then calculate how the existing threats, and barriers to those threats, affect this quartile:
### Threat 1
predationAdult <- function(init, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) {
  init * avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey
}

predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}

### Threat 2
predationJuv <- function(init, avoid) {
  init * avoid
}

predationJuvRev <- function(TopEventFreq, avoid) {
  TopEventFreq/( avoid )
}

### Threat 3
habitatAppropriation <- function(init, setAsides, recoveryRestoration, afforestation) {
  init * setAsides * recoveryRestoration * afforestation
}

### Threat 4
stress <- function(init, managementForFood, dailySelection, stableEpidemiology, resistance, preventHarassment) {
  init * managementForFood * dailySelection * stableEpidemiology * resistance * preventHarassment
}

#########################################################################################################################################################
#########################################################################################################################################################
# Rational for populating the BRAT framework
#########################################################################################################################################################

# Determining what claues should go into the Initial Frequency of a Threat (Blue boxes), and what values should go into the barriers to a threat (White
# boxes) can be tricky and subjective in the BRAT framework. The values need to come from the best sourced data, and when those data are limited, they
# can be hard to determine. However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
# available. Our manuscript here is show how, and can be changed as new information comes to light - not only the number of threats, barriers to a threat,
# can be changed, but the associations between these items can be changed too.


# Rationale for calculated threat vlaues
# These thoughts are also documented in 'Quantification questions_FScomments.docx'


# Threat 1 - General predation
### Current Top Event Frequency
Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality

### Initial Frequency
##### calculated from the BRAT threat line:
predationAdultRev <- function(TopEventFreq, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) { # calculate the initial frequency of predation given the current frequency and the values of the thresholds
  TopEventFreq/ (avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey)
}
Threat1_InitialFreq<- predationAdult(Threat1_topEvent, 2, 1.5, 0.65, 1.1, 1, 0.9)


# Threat 2 - juvenile predation
### Initial Frequency
##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is not a factor)
# Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
Threat2_InitialSurv<- pregR*SadF*surv1stDay # proportion of females that produce a calf
Threat2_InitialFreq<- (1 - (Threat2_InitialSurv)) # proportion of females that loose a calf

### Current Top Event Frequency
##### Difference between the calculated Initial Frequency, and the ECCC reported value (i.e. how many calf deaths from day 1 to a population survey are unaccounted for?)
Difference = Mortrecr - Threat2_InitialFreq 
# We know from maternal penning data that even between day 2 and day 30, only 90% of calves survive (i.e. even without predation, 90% of calves survive, and 10% die from "other causes; McNay work)
## Assume this rate remains consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
Difference_predation = Difference*0.9 
# Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
Threat2_topevent <- Difference_predation + Threat2_InitialFreq

### Barrier value: Effect of predator avoidance
##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
Threat2_barrier <- Threat2_topevent/Threat2_InitialFreq


# Needed for the rest of the framework:
### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
Difference_nonpredation = Difference - Difference_predation
### Therefore, we need to add the multiplier of 1/2 this rate to the final two Threats, to account for the proportion directly attributable to juveniles
### Remember at the start of this script I already specified Threat multipliers. You can change them there if needed.
Threat3_multiplier
Threat4_multiplier


# Threat 3: Permanent habitat appropriation
### Current Top Event Frequency
Threat3_topevent = Threat3_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat3_InitialFreq <- Threat3_multiplier*(prod(0.95* 1* 1))


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat4_InitialFreq <- Threat4_multiplier*(prod(1, 0.8, 1, 1, 1))
########################################################################################################################################################


# Step 6: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) vlaues
# these values can be changed to better reflect data as it becomes available:
### the first number of each function is the probability that the threat affects the citical event (lambda below zero)
### the subsequent numbers of each function are the probability that the barrers will FAIL to prevent the threat

# Top Event Frequency Function
# calculate the top event frequency, before mitigation actions take place.
topEvent <- function(...) {
  sum(unlist(list(...)))
}

# Consequency Frequency Function
# Calculate lambda after mitigation actions take place
mitigate <- function(cull, dd, pens){
  cull * dd * pens
}


topEvent <- topEvent(predationAdult(Threat1_InitialFreq, 2, 1.5, 0.65, 2, 1, 0.9), # climate change scenario
                     predationJuv(Threat2_InitialFreq, 2.19), 
                     habitatAppropriation(Threat3_InitialFreq, 0.95, 2, 1), 
                     stress(Threat4_InitialFreq, 2, 2, 2, 1, 1))
###

postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(1, 1, 0.95)) # this is the maternity pen lever - acting solo
#postMitigate <- postMitigate(topEvent, mitigate(0.5, 1, 1)) # this is wolf cull lever - acting solo.
#postMitigate <- postMitigate(topEvent, mitigate(0.5, 1, 0.95)) # combined mitigation/normal scenario
postMitigate <- postMitigate(topEvent, mitigate(1, 1, 1)) # Climate change scenario


# Step 7: look at the hazard and consequence values, and compare
message("Lambdas: ")
print( (1 - mortQuantile) + 1) # this should be the Hazard target frequency
print( (1 - topEvent) + 1) # this should be the hazard current total top event frequency
print( (1 - postMitigate) + 1) # this is the current consequence frequency

#message("Lambdas: ")
#print(mortQuantile)  # this should be the Hazard target frequency
#topEventN <- ((1 - topEvent) 
#print(topEventN)  # this should be the hazard current total top event frequency
#postMitigateN <- (1 - postMitigate)
#print(postMitigateN) # this is the current consequence frequency


# Print associated lopa crit messages by comparing these values:
out1 <- ifelse(topEvent >= mortQuantile, "Red - caribou population growthrate is persistently below 1", "Green - caribou population growthrate is above 1")
out2 <- ifelse(postMitigate >= mortQuantile, "Red - caribou extirpated", "Green - caribou recovered")
print(out1)
print(out2)
# Your solution!











