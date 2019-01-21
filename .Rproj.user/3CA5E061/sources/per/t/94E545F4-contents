#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# January 2019
# Code by Eliot McIntire, annotation by Frances Stewart
#################################################################################

# Objectives: ----
### Calculate target frequency for the critical event
##### Policy says the target frequency should be 40% (Environment and Climate Change Canada 2012 Recovery Strategy) - ECCC has indicated landscape change should
##### not exceed 35%, as this produces a 60% probability of herd persistence. In otehr words, this policy threshold is stated at 40% risk of population decline
##### for any specific population. Hence for 40%
####### But this 40% is obtained from a normal distribution - i.e. it is the area under a normalized curve (bc the models in ECCC2012 assume a normal distribution)
####### representing the lower 40% of the distribution.

# Our question is:
### how much mortalilty produces a lambda that is less than the 40% of this distribution?

N = 100 # number of adult females in the population
SadF <- 0.88 # Adult female survival (# adult females alive in spring). Inverse is the initial risk for predation on Adult females (blue box)
recr <- 0.34 # Recruitment (# calves: # cows in spring). Inverse is the initial risk for predation on calves (blue box)

# Population model ----
###################################################################
# we dont actually need this part right now. Skip to the next part
###################################################################
# calculating the number of animals in the poplation after so many years
# does not take density dependence into account

if (FALSE) {
  MadF <- 1 - SadF # mortality of adult females
  pregR <- 0.9 # pregnancy rate of adult females
  surv1stDay <- 0.8 # survival of calf to 1st day
  mort1stDay <- 1 - surv1stDay # mortality of calves within 1st day
  survToSurvey <- recr / prod(SadF, pregR, surv1stDay) # survival of a calf to the survey time
  mortToSurvey <- 1 - survToSurvey # mortality of calves before the 1st survey
  
  
  annual <- function(N, SadF, recr) { # annual estimation of N
    newN <- N * SadF
    newN <- newN + newN * (recr / 2)
    round(newN, 0)
  }
  
  Nyears <- 100 # number of years you want to look at
  N <- integer(Nyears)
  N[1] <- 100
  for (yr in 1:Nyears) {
    N[yr + 1] <- annual(N[yr], SadF, recr)
  }
  print(N)
}
##################################################################
##################################################################

# Critical Threshold calculation ----
sumMort <- function(SadF, recr) { # total female population mortality
  1 - SadF + (1 - (recr / 2)) # divide by 2, as half of the animals recruiting will be male
}

sd1 <- 0.1
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd
# NOTE! we do not know the SD (vairation) of LPUs. The 0.1 is a placeholder for now.
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 
# Assign this value of the distribution to the Target Frequency of the hazard box (black and yellow).

ifelse(mortQuantile >= sumMort(SadF, recr), "Green", "Red")
# if the 40% mortality quartile is higher than the sum of all adult female and juvenile female mortaility, then lambda will be less than 1 (the top event)
### green means population is good to go
### red means mortality is too high
# i.e. the total female mortality cannot exceed 40% of the probability distribution surrounding lambda.


####################################################################
####################################################################
# Case studies using 2008 data ----

# Snake-Sahtaneh (BC)
N = 100 
SadF <- 0.94 
recr <- 0.072

# population model:
#> print(N)
#[1] 100  97  94  92  90  88  86  84  82  80  78  76  74  72  70  68  66  64  62  60  58  56  55  54  53  52  51  50  49  48  47  46  45  44  43  42  41  40
#[39]  39  38  37  36  35  34  33  32  31  30  29  28  27  26  25  24  23  22  21  20  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19
#[77]  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19  19

# Critical threshold calculation:
#> ifelse(mortQuantile >= sumMort(SadF, recr), "Green", "Red")
#[1] "Red"
# mortality is too high - probabbilty due to recruitment!
# corresponds with ECCC 2011 status of "not self sustaining"

# by how much?
Diff = mortQuantile - sumMort(SadF, recr)
#-0.04

# Val D'Or (QC)
N = 100 
SadF <- 0.87
recr <- 0.153

# population model:
#>   print(N)
#[1] 100  94  88  82  77  72  67  63  59  55  52  49  46  43  40  37  35  33  31  29  27  25  23  22  21  20  19  18  17  16  15  14  13  12  11  10   9   8
#[39]   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7
#[77]   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7   7

# critical threshold calculation:
#> ifelse(mortQuantile >= sumMort(SadF, recr), "Green", "Red")
#[1] "Red"

# by how much?
Diff = mortQuantile - sumMort(SadF, recr)
# -0.078

######################################################################
# Case studies using 2011 data ----
# Sadf is alwasy assumed to be 0.85

# Chinchaga (BC) ----
N = 100 
SadF <- 0.85
recr <- 0.139

# population model:
#>   print(N)
#[1] 100  91  83  75  68  62  56  51  46  42  38  35  32  29  26  24  22  20  18  16  15  14  13  12  11  10   9   8   7   6   5   5   5   5   5   5   5   5
#[39]   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
#[77]   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5

# critical threshold calculation
#> ifelse(mortQuantile >= sumMort(SadF, recr), "Green", "Red")
#[1] "Red"

# by how much?
Diff = mortQuantile - sumMort(SadF, recr)
# -0.106

# this is depressing
# Let's do one of the herds with the highest recruitment:

# Manicouguan (QC) ----
N = 100 
SadF <- 0.85
recr <- 0.503

# population model:
#phew!
#>   print(N)
#[1]   100   106   113   120   128   136   145   154   164   174   185   197   210   223   237   252   268   285   303   322   343   365   388   413   439
#[26]   467   497   529   563   599   637   678   721   767   816   868   923   982  1045  1112  1183  1258  1338  1423  1514  1611  1714  1823  1939  2063
#[51]  2195  2335  2484  2642  2810  2989  3180  3383  3599  3829  4073  4333  4609  4903  5216  5549  5903  6279  6679  7105  7558  8040  8553  9098  9678
#[76] 10295 10952 11650 12393 13183 14024 14918 15869 16881 17958 19103 20321 21617 22996 24463 26023 27683 29448 31326 33324 35449 37710 40115 42673 45394
#[101] 48289

# critical threshold calculation
#> ifelse(mortQuantile >= sumMort(SadF, recr), "Green", "Red")
#[1] "Green"

# by how much?
Diff = mortQuantile - sumMort(SadF, recr)
# 0.07


######################################################################################################
######################################################################################################
# Eliot's code for the BRAT diagram (Winder et al. 2018 in prep; as of January 16th 2019)
# each function applies the threat and barriers to the threat for each branch of the diagram.
######################################################################################################
# BRAT code ----

# you can find a general description of the BRAT diagram components at: https://www.cgerisk.com/knowledgebase/The_bowtie_method


# BLUE SQUARES ON LEFT SIDE OF FIGURE (ie. the threats)
## each barrier to a threat (the white circles) are included here as variables
## the quantification of each variable is: the probability that the barrier will FAIL to stop the threat

# Threat #1: predation on adult females (i.e. SadF)
predationAdult <- function(init, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) {
  init * avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey
}

# Threat #2: predation on juveniles (i.e. Rec)
predationJuv <- function(init, avoid) {
  init * avoid
}

# Threat #3: habitat appropriation on adults (and by default also juveniles)
habitatAppropriation <- function(init, setAsides, recoveryRestoration, afforestation) {
  init * setAsides * recoveryRestoration * afforestation
}

# Threat #4: Stresses affecting health of adult females (and by default also juveniles)
stress <- function(init, managementForFood, dailySelection, stableEpidemiology, resistance, preventHarassment) {
  init * managementForFood * dailySelection * stableEpidemiology * resistance * preventHarassment
}


# RED CIRCLES
# projected collapse of boreal caribou population (i.e. the consequence)
mitigate <- function(cull, dd, pens){
  cull * dd * pens
}

# hazards leading into the top event
topEvent <- function(...) {
  sum(unlist(list(...)))
}


#### CALCULATIONS ###

# each event (top and mitigate) is a product of all the previous components
topEvent <- topEvent(predationAdult(1-SadF, 2, 1.5, 0.65, 1.1, 1, 0.9), # SadF will be herd specific. Average as of ECCC 2011 is: 0.837
         predationJuv(1-recr, 1), # recr will be herd specific. Average as of ECCC 2011 is: 0.268 
         habitatAppropriation(.15, 0.95, 1, 1),
         stress(0.1, 1.5, 0.8, 1, 1, 1))


postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

postMitigate <- postMitigate(topEvent, mitigate(0.81, 1, 0.95))

### FINAL VALUES (i.e. Lopa Crit) ###
print(topEvent)
print(postMitigate)

# we then need to compare the mortQuartile value to this
# if Mort Quartile is greater than the topEvent value, then the caribou growth rates (lambda) are already below zero
# if mort Quartile is greater than the post Mitigate value, then even with mitigations ("recovery actions/levers") the caribou will still become extirpated.


###########################
###########################
# An entire example - Chinchaga herd (STUDY AREA 3)---- 
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
N[1] <- 100
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

sd1 <- 0.1 # this value can go as high as 0.3, from the literature.
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd of sd1
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 


# Step 4: put this mortQuantile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print( (1 - mortQuantile) + 1)


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
Threat1_InitialFreq<- predationAdultRev(Threat1_multiplier, 2, 1.5, 0.65, 1.1, 1, 0.9)


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
Threat3_InitialFreq <- habitatAppropriation(Threat3_multiplier, 0.95, 1, 1)


# Threat 4: Stresses reducing caribou fitness and health
### Current top event frequency
Threat4_topevent <- Threat4_multiplier

### Initial Frequency
# multiply it by what values exist in the barrier to back calculte this value
Threat4_InitialFreq <- stress(Threat4_multipler, 1.5, 0.8, 1, 1, 1)
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

### START HERE ----
topEvent <- topEvent(predationAdultRev(Threat1_InitialFreq, 2, 1.5, 0.65, 1.1, 1, 0.9), # this is currently WRONG, each of the threat multiplier values
                     predationJuvRev(Threat2_InitialFreq, 2.2), # need to be replaced with the Initial values - but when I try this I get green, green
                     habitatAppropriation(Threat3_InitialFreq, 0.95, 1, 1), # need to go back to the BRAT docx and double check the values.
                     stress(Threat4_InitialFreq, 1.5, 0.8, 1, 1, 1))
#### START HERE ---

postMitigate <- function(topEvent, mitigate) {
  topEvent * mitigate
}

postMitigate <- postMitigate(topEvent, mitigate(0.81, 1, 0.95))


# Step 7: look at the hazard and consequence values, and compare
message("Lambdas: ")
print( (1 - mortQuantile) + 1) # this should be the Hazard target frequency
print( (1 - topEvent) + 1) # this should be the hazard current total top event frequency
print( (1 - postMitigate) + 1) # this is the current consequence frequency


# Print associated lopa crit messages by comparing these values:
out1 <- ifelse(topEvent >= mortQuantile, "Red - caribou population growthrate is persistently below 1", "Green - caribou population growthrate is above 1")
out2 <- ifelse(postMitigate >= mortQuantile, "Red - caribou extirpated", "Green - caribou recovered")
print(out1)
print(out2)
# Your solution!

# CURRENTLY NOT WORKING!!
# RE-TRY AFTER ANDREW GIVES US NEW DATA.







