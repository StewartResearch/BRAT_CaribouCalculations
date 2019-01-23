#################################################################################
# Figuring out Caribou calculations
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

Threat1_InitialFrequency = prod(Threat1_multiplier, (2, 1.5, 0.65, 1.1, 1, 0.9))
# or, as specified above:
predationAdult <- function(Threat1_CurrentTopEventFrequency, avoid, seismic, huntPred, earlySeral, huntCaribou, huntAltPrey) {
  init * avoid * seismic * huntPred * earlySeral * huntCaribou * huntAltPrey
}

# Juvenile recruitment 
# what proportion of calves should survive if there are no predators? (Initial frequency)
PerCapRecr<- 0.9*SadF*surv1stDay
PerCapMort <- 1- PerCapRecr
#0.37

# but, from maternal penning experiments, we know that roughly 90% of calves survive if there are no predators. (10% death)
# ECCC is saying that 87% of calves are not surviving
# What would this number be if there was no predation?
surv1stDay <- 0.8
surv30thDay <- 0.9
survNoPred <- surv1stDay*surv30thDay
mortNoPred = 1-survNoPred
Pred <- (1-recr)-mortNoPred
# 0.59 # predation accounts for 0.59 of the recruitment

# the effect of predators is therefore 0.59/0.37 = 1.59


#################################################################
# January 17th ----
# re-trial
###############
# using Chinchaga herd data from ECCC 2011

# Step 1: Have some data
N = 250 # population of the Chinchaga herd as of 2008
SadF = 0.87 # this is what we currently know 
recr = 0.13 # this is what we currently know 

# Step 2: first calculate the population growth distribution
pregR <- 0.9 # pregnancy rate of cows
surv1stDay <- 0.8 # survival rate of calves through their 1st 24 hours of life (from maternity pen experiments)
mort1stDay <- 1 - surv1stDay # mortality of calves in their 1st 24 hours
survToSurvey <- recr / prod(SadF, pregR, surv1stDay)
mortToSurvey <- 1 - survToSurvey 


annual <- function(N, SadF, recr) { # annual population estimation function given ECCC's values
  newN <- N * SadF
  newN <- newN + newN * (recr / 2)
  round(newN, 0)
}

Nyears <- 100 # number of years you want to look at.
N <- integer(Nyears)
N[1] <- 100
for (yr in 1:Nyears) {
  N[yr + 1] <- annual(N[yr], SadF, recr)
}
print(N)

# Step 3: then calculate the 40% threshold of this distribution
sumMort <- function(SadF, recr) { 
  1 - SadF + (1 - (recr / 2)) 
}

sd1 <- 0.1
b <- rnorm(1e5, 1, sd1) # make a normal distribution, with a sd
# NOTE! we do not know the SD (vairation) of LPUs. The 0.1 is a placeholder for now.
mortQuantile <- qnorm(0.4, mean = 1, sd1) # mortality quartile is at the 40% mark of this distribution. 


# Step 4: put the mortQuantile into the Target frequency of the Hazard box
# in this case it is:
print( (1 - mortQuantile) + 1)

# 1.02


