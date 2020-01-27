#################################################################################
# Caribou calculations
# for BRAT paper: Winder et al. in prep at Frontiers in Ecology and Evolution
# Started: January 2019
# Last revised: November 27, 2019
# authors: Eliot McIntire and Frances Stewart
#################################################################################

# There are three functions you will need to call to complete this BRAT analysis.

## First, run the BRAT using BRAT()
### Many of the parameter values needed for this function, and the function code itself, are described in BRATfunction.R

## Then look at mitigation options (the right side of the BRAT diagram) using the Mitigation()
### The parameter values and the the function itself is contained in Mitigationfunction.R

## Finally, you can produce a figure of both the BRAT and Mitgation values using the Plot()
### Run this function from Plotfunction.R

#####
# Blow are some of the demographic values we used for each of the herds we investigated. You can substitute out 
# these demographic values to include vlaues for other herds if you'd like to conduct this analysis in other
# study areas.

# For Chinchaga: data obtained from the ECCC 2011 scientific report
#N = 250 # population of the Chinchaga herd as of 2008
#SadF = 0.87 # Adult female survival 
#recr = 0.13/2 # Juvenile female recruitment (assuming a 50:50 sex ratio) 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

# snake-sahtahneh herd (STUDY AREA 3) 
#N = 360 # population of the Chinchaga herd as of 2008
#SadF = 0.94 # Adult female survival 
#recr = 0.072/2 # Juvenile female recruitment 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.


# and An "averaged" population
# ASSUMPTION: No one herd demography is accurate for a study area. Instead, we quantify the BRAT analysis at the meta-region.
#N = (360 +  250)/2 # population of the Chinchaga herd as of 2008
#SadF = (0.94 + 0.87)/2 # Adult female survival 
#recr = ((0.072 + 0.139)/2)/2 # Juvenile female recruitment 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.


#####
# Bleow is also all the inital exploratory code that went into making the above three functions. 
# I'll keep it here as a reference, incase a file is lost or deleted. 

### For Chinchaga: data obtained from the ECCC 2011 scientific report
#N = 250 # population of the Chinchaga herd as of 2008
#SadF = 0.87 # Adult female survival 
#recr = 0.13/2 # Juvenile female recruitment (assuming a 50:50 sex ratio) 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.

# snake-sahtahneh herd (STUDY AREA 3) 
#N = 360 # population of the Chinchaga herd as of 2008
#SadF = 0.94 # Adult female survival 
#recr = 0.072/2 # Juvenile female recruitment 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.


# and An "averaged" population
# ASSUMPTION: No one herd demography is accurate for a study area. Instead, we quantify the BRAT analysis at the meta-region.
#N = (360 +  250)/2 # population of the Chinchaga herd as of 2008
#SadF = (0.94 + 0.87)/2 # Adult female survival 
#recr = ((0.072 + 0.139)/2)/2 # Juvenile female recruitment 
#sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
# 
# 
# BRAT<-function(N, SadF, recr, sd1,  multiplier1, pregR, sexRatio, wolfCullEffect, wolfCullPropOnAdults){
# # variables
#   ## N = herd population size (EC 2012)
#   ## SadF = adult female survival (EC 2012)
#   ## pregR 0.9 - from Scott McNay - roughly 90% of adult females are pregnant each year
#   ## sexRatio = 0.5
#   
# #variables that we can change:
#   ## sd1 - this was initially 0.1 (from Sutherland sensitivity analysis)
#   ## multiplier 1 - this was initially 90% (90% of adult female mortality is due to Threat line 1)
#   ## wolfCullEffect - this is from Hervieux et al 2014 and could change with more information <- 0.186
#   ## wolfCullPropOnAdults - we assumed that the majority of a wolf cull evect ws on juveniles <- 0.1
# 
#   
#  
# # Objectives: ----
# ### Calculate target frequency for the critical event
# ##### Policy says the target frequency should be 40% (Environment and Climate Change Canada 2012 Recovery Strategy) 
# ## - ECCC has indicated landscape change should not exceed 35%, as this produces a 60% probability of herd persistence. 
# ## In other words, this policy threshold is stated at 40% risk of population decline for any specific population.
# # But this 40% is obtained from a normal distribution 
# ## - i.e. it is the area under a normalized curve (because the models in ECCC2012 assume a normal distribution)
# ## representing the lower 40% of the distribution.
# 
# ## Overall, we want to determine if a herd is above the 40% threshold (ie in the 60% probability of survival category)
# ## We therefore set 60% as our target threshold to achieve.
# 
# ## Through out this script, assumptions are stated as "ASSUMPTION". Search for this word too see them all.
# 
# 
# ###########################
# ###########################
# # Chinchaga herd (STUDY AREA 1) ----
# ###########################
# 
# #########
# # Set the barriers here -- this would be the numbers that go into the BRAT figure
# # you might have to conduct several of the below steps before dpoing this one, if the barriers are not estimated from literature.
# Threat1_barriers <- c(quote(Threat1_barrier_1), 1.4, 0.65, 1.5, 1.0, 0.9, 1.0) # in units of Initial Frequency
# Threat2_barriers <- quote(c(Threat2_barrier_1)) # in units of Initial Frequency # This is derived from data below
# Threat3_barriers <- c(1.00, 1.00, 1.00) # in units of Initial Frequency
# Threat4_barriers <- c(1.05, 1.05, 1.05, 1.00, 1.0)
# 
# ###############################################################################
# # Step 1: know (or estimate) some basic information about each study area/herd. ----
# ### average demographic values from Snake and Chinchaga as of 2008 - from downloadable csv online:
# # source: https://open.canada.ca/data/en/dataset/4eb3e825-5b0f-45a3-8b8b-355188d24b71
# 
# # Chinchaga
# 
# # Step 1: know (or estimate) some basic information about each study area/herd. 
# ### For Chinchaga: data obtained from the ECCC 2011 scientific report
# #N = 250 # population of the Chinchaga herd as of 2008
# #SadF = 0.87 # Adult female survival 
# #recr = 0.13/2 # Juvenile female recruitment (assuming a 50:50 sex ratio) 
# #sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
# 
# # invert these numbers to calculate the mortality values:
# MortSadF = (1 - SadF) # part of this will go into the Initial frequency of the threats
# Mortrecr = (1 - recr/2) # part of this will go into the Initial frequency of the threats
# 
# 
# 
# ################################
# # Step 2: State your assumptions ----
# ### In this example, there are 4 threats. 
# ### Three of these threats apply to adult females, and three of these threats apply to juveniles
# ### One threat each applies to only adult females and juveniles
# ##### We need to separate out a portion of the above mortality measures for each of the frequencies 
# ## (whether initial, or current top event) for each threat.
# 
# ##### The literature generally says that most of the mortality is due to predation, 
# ## and that the majority of this is on juveniles (Hervieux et al. 2014; Wittmer et al. 2005 a and b). 
# #####
# 
# Threat1_multiplier = MortSadF * multiplier1 # ASSUMPTION # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
# #Threat2_multiplier = We calculate this amont below, and name it 'Difference_nonpredation', so we do not include it here.
# multiplier2 = (1 - multiplier1)/2
# Threat3_multiplier = MortSadF * multiplier2# ASSUMPTION # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
# Threat4_multiplier = Threat3_multiplier # ASSUMPTION
# 
# # Assumptions about caribou population demongraphics, from published literaure, reports, and pers. comms:
# # These values are used to calculate the current top event frequency, for juveniles
# # pregnancy rate
# # pregR <- 0.9 # DATA/ASSUMPTION # pregnancy rate - from observations in maternal pens. 
# # (Scott McNay pers com, Kinse-Za maternal penning project)
# # also similar to observations in northeastern alberta (McLoughlin et al. 2003; obtained from serum collected during capture)
# # juvenile survival to the fist day of life
# 
# #sexRatio<- 0.5 # only half of the animals born will be female 
# surv1stDay <- 0.8 * sexRatio # DATA/ASSUMPTION # juvenile female survival to the first day - from maternal pen observations 
# # (Scott McNay pers com, Klinse-Za maternal penning project)
# # also similar to observations in northeastern alberta (McLoughlin et al. 2003)
# 
# mort1stDay <- 1 - (surv1stDay)  
# 
# ###########################################################################################################
# # Step 3: calculate a normal distribution, and indicate the 60% value ----
# ### This 60% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
# ### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
# ### In other words, we are OK with 40% of the herds dissapearing, on average.
# 
# lambdaQuartile <- qnorm(0.6, mean = 1, sd1) # mortality quartile is at the 60% mark of this distribution. 
# # in other words, what lambda vlaue do herds need to accomplish to obtain sustainability according to ECCC's 2012 40% threshold.
# 
# # Put this lambdaQuartile into the Target frequency of the Hazard box
# ### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# # in this case it is:
# print(lambdaQuartile) 
# # 1.025
# 
# ##################################################################################
# # Step 4:populating the BRAT framework Initial frequency and Current top events of threats ----
# # Step 4 thorugh 6 are summarized in Table 2 of the manuscript (Winder et al. 2019)
# 
# # Determining what vlaues should go into the Initial Frequency of a Threat (Blue boxes), and what values should 
# # go into the barriers to a threat (White boxes) can be tricky and subjective in the BRAT framework. 
# # The values need to come from the best sourced data, and when those data are limited, they  can be hard to determine.
# 
# # However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
# # available. Our manuscript here is showhow, and can be changed as new information comes to light - not only the number 
# # of threats or barriers to a threat, can be changed, but the associations between these items can be changed too.
# 
# ####### Current top event frequencies are from known data 
# ## (ie. known mortality or recruitment from Environment Canada 2008; Environment Canada 2012 data)
# ####### Initial event frequencies are things that we need to find out (i.e. calculate)
# ## (ie. what was the frequency of this threat if there had been no landscape changes)
# 
# 
# # Threat 1 - General predation
# ### Current Top Event Frequency
# # some herds have very low adult female mortality. Thorugh trial and error with Snake and the Averaged herd, we discovered that
# # if MortSadF is less than 0.1 (i.e less than 10% of adult females are dying) the effectiveness of wolf controls was listed as > 100%.
# # This just didnt make any sense. We therefore assume # ASSUMPTION # that 10% represents an ecological threshold of other processes
# # dominating adult female survival. We discovered that if we limit the initial frequency of adult female mortality to only 40%
# # by predation, the effectiveness of wolf controls made sense (i.e. less than 100%) across all study situations. We therefore assume
# # that if less than 10% of females are dying, 4% will be due to wolf predation, and the other 6% will be due to other causes (highlighted
# # in threats 3 and 4).
# if(MortSadF < 0.10) {
#   Threat1_InitialFreq <- MortSadF*0.4
# 
#   } else { # if not, we back-calculated the required threat value:
# Threat1_InitialFreq <- 0.05549 # ASSUMPTION
# # This calculation involved back calculating from the wolf mitigation initial frequency (0.814), to the juvenile predation threat frequency,
# # translating this value into lambda units of wolf predation and compensatory predation, calculating the ratio between these
# # predation effects, and applying the ratio to adult wolf vs compensatory predation. From there we could substitute in the current
# # top event frequency (known from Environment Canada data), to get this inital frequency value.
# # ASSUMPTION: the effectiveness of wolfculls extends from juveniles to adults equally
# # ASSUMPTION: This is a constant applied to all populations to determine the threat values of adult predation (as long as adult female
# # mortality is > 10%. See above portion of the IF statement). 
# ### This might be a big assumption! But it is mitigated by the above IF statement.
# ###### See how we calculated this at the end of the script ('Other Information').
# }
# 
# 
# Threat1_barrier_1 <-  Threat1_multiplier / (Threat1_InitialFreq * prod(unlist(Threat1_barriers[-1])))
# Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality
# Threat1_InitialFreq <- Threat1_topEvent/prod(sapply(Threat1_barriers, eval))
# 
# 
# 
# # Threat 2 - Juvenile predation
# ### Initial Frequency
# ##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is a factor)
# # Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
# Threat2_InitialSurv <- pregR * SadF * surv1stDay # proportion of females that produce a femalecalf
# Threat2_InitialFreq <- (1 - (Threat2_InitialSurv)) # proportion of females at the start of a year that result in no female calf a year later
# 
# ### Current Top Event Frequency
# ##### Difference between the calculated Initial Frequency, and the Environment Canada reported value 
# #   (i.e. how many female calf deaths from day 1 to a population survey are unaccounted for?)
# PropMortDay30ToSurvey = Mortrecr - Threat2_InitialFreq 
# # We know from maternal penning data that even between day 2 and day 30, 90% of calves survive 
# #   (i.e. even without predation, 90% of calves survive, and 10% die from "other causes"; Scott McNay pers comm - Klinse-Za meternity pen)
# ## Assume this rate is consistent from day 2 to a population survey - MIGHT BE A BIG ASSUMPTION!
# PropMortDay30ToSurvey_predation <- (PropMortDay30ToSurvey * 0.9) # DATA/ASSUMPTION #
# # Mortality due to predation, plus mortality due to other causes will be the Current Top Event Frequency:
# Threat2_topevent <- PropMortDay30ToSurvey_predation + Threat2_InitialFreq 
# 
# ### Barrier value: Effect of predator avoidance
# ##### Because this is the only barrier to the threat of predation that we have identified, we can now easily calculate this number
# Threat2_barriers <- Threat2_topevent / Threat2_InitialFreq
# Threat2_barrier_1 <- Threat2_barriers[1]
# 
# 
# # Needed for the rest of the framework:
# ### From above, we can calculate the proportion of juvenile mortalities that are NOT due to predation
# Difference_nonpredation = PropMortDay30ToSurvey - PropMortDay30ToSurvey_predation
# ### We need split this rate to the final two Threats, to account for the proportion of non-predation mortality
# # directly attributable to female juveniles
# 
# 
# # Threat 3: Permanent habitat appropriation
# ### Current Top Event Frequency
# Threat3_topevent = Threat3_multiplier + (0.5 * Difference_nonpredation) # DATA/ASSUMPTION #### ----
# 
# ### Initial Frequency
# # multiply it by what values exist in the barrier to backcalculte this value
# Threat3_InitialFreq <- Threat3_topevent/(prod(Threat3_barriers))
# 
# 
# # Threat 4: Stresses reducing caribou fitness and health
# ### Current top event frequency
# Threat4_topevent <- Threat4_multiplier + (0.5 * Difference_nonpredation) # DATA/ASSUMPTION #### ----
# # assumed mortality proportion for adult females, plus, what we know of juvenile mortality minus 50% of what we dont know about juvenile mortality
# 
# ### Initial Frequency
# # multiply it by what values exist in the barrier to back calculte this value
# # Threat4_InitialFreq <- Threat4_multiplier*(prod(0.95, 0.950, 0.95, 1.00, 1.00))
# Threat4_InitialFreq <- Threat4_topevent/(prod(Threat4_barriers))
# 
# #######################################################
# # step 5: Convert the inital frequency values to values of lambda ----
# 
# Threat_LambdaEffect <- list()
# Threat_LambdaEffect[[1]] <- (Threat1_InitialFreq * sapply(Threat1_barriers, eval)) - Threat1_InitialFreq # in Lambda units
# Threat_LambdaEffect[[2]] <- Threat2_InitialFreq * Threat2_barriers - Threat2_InitialFreq # in Lambda units
# Threat_LambdaEffect[[3]] <- Threat3_InitialFreq * Threat3_barriers - Threat3_InitialFreq # in Lambda units
# Threat_LambdaEffect[[4]] <- Threat4_InitialFreq * Threat4_barriers - Threat4_InitialFreq # in Lambda units
# 
# # wolfCullEffect <- 0.186 # DATA/ASSUMPTION 
# # i.e. lambda has the potential to increase by 18.6% during a full wolf cull.
# # Hervieux et al. (2014) found lambda increased between 4.6 (within herds) and 14% (between herds) during a 50% wolf cull. 
# # We avereaged these fundings, and standardized by 50% wolf cull to get the 18.6%
# # this assumes a linear relationship with no threshold effects # ASSUMPTION #
# 
# #############################################################################################################################
# # Step 5b
# # we now wanted to split this up between the effect of predation by wolves, and the effect of predation as a compensatory effect 
# # (or "other" predators, OR wolves that are not being culled)
# # i.e. this could include wolf immigration and source/sink dynamics -> an important discussion point.
# 
# # We wanted to state this in terms of lambda units
# Threat_LambdaEffect[[2]]
# #  So, 0.444 is predator effect on calves -- partition into "non-compensated wolf" == "True effect of wolves on calves" 
# #     and "other" e.g., 0.186 is max possible... partition amongst adults and juvs e.g., 0.9 and 0.1 -->
# #     0.9 * 0.186 on calves and 0.1 * 0.186  on adults
# #  "other predation" for adults = 0.08775 - (0.1 * wolfCullEffect) = 0.07375 in Lambda units
# #     i.e. Threat_LambdaEffect[[1]] - (0.1*wolfcullEffect)
# #  "other predation" for adults = 
# 
# # ASSUMPTION # we assumed that the majority of predation happend on juveniles (Hervieux et al. 2014) - 90% of predation happened on juveniles
# # wolfCullPropOnAdults <- 0.1
# wolfCullPropOnJuvs <- 1 - wolfCullPropOnAdults
# # therefore: 
# wolvesOnAdults <- wolfCullPropOnAdults * wolfCullEffect # Lambda units
# otherOnAdults <- Threat_LambdaEffect[[1]][1] - wolvesOnAdults # Lambda units
# 
# # Percent effectiveness on adult recruitment:
# effectivenessAdults<- wolvesOnAdults/(otherOnAdults + wolvesOnAdults)
# 
# 
# #  "other predation" for calves = 0.444584 - 0.9 * wolfCullEffect = 0.277 in Lambda units
# #     i.e. Threat_Lambda[[2]] - (0.1*wolfcullEffect)
# #  "other predation" for calves = 
# wolvesOnJuvs <- wolfCullPropOnJuvs * wolfCullEffect # Lambda units
# otherOnJuvs <- Threat_LambdaEffect[[2]][1] - wolvesOnJuvs # Lambda units
# 
# # Percent effectiveness on juvenile recruitment:
# effectivenessJuvs <- wolvesOnJuvs/(otherOnJuvs + wolvesOnJuvs)
# 
# # Adults
# allOnAdults <- wolvesOnAdults/effectivenessAdults
# otherOnAdults <- allOnAdults - wolvesOnAdults
# 
# # Double checking out calculations:
# a <- (otherOnAdults + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1] # Lambda units
# #   In InitialEvent units --> (0.277 + Threat2_InitialFreq) / Threat2_InitialFreq
# b <- (otherOnJuvs + Threat2_InitialFreq[1]) / Threat2_InitialFreq[1] # Lambda units
# #   Wolf part In InitialEvent units --> ((0.0186 - 0.07375) + Threat1_InitialFreq) / Threat1_InitialFreq
# d <- ((wolvesOnAdults) + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1] # Lambda units
# #   Wolf part In InitialEvent units --> ((0.444584 - 0.3185) + Threat2_InitialFreq) / Threat2_InitialFreq
# # Wolf part In InitialEvent units --> 
# f <- ((wolvesOnJuvs) + Threat2_InitialFreq) / Threat2_InitialFreq # Lambda units
# 
# # This should be the effect on adults in initial units
# Threat1_barrier_1 <- (a - 1)  + (d - 1)  + 1 # in initial units
# # this should be the effect on Juvniles in initial units 
# (b - 1)  + (f - 1)  + 1 # in initial units 
# # Sum of compensatory causes and non-compensatory causes 
# #  of "the predation effect", which is PropMortDay30ToSurvey_predation
# 
# # should be the same as equal:
# sapply(Threat1_barriers, eval)[[1]] # :)
# # also, lambda values should equal
# #Threat1_LambdaEffect = wolvesOnAdults + otherOnAdults # check point if desired
# #Threat2_LambdaEffect = wolvesOnAdults + otherOnJuvs # check point if desired
# 
# ########################
# # Step 6: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) values ----
# # these values can be changed to better reflect data as it becomes available:
# 
# # Top Event Frequency Function
# # calculate the top event frequency, before mitigation actions take place.
# topEventCalculator <- function(...) {
#   sum(unlist(list(...)))
# }
# 
# # Consequency Frequency Function
# # Calculate lambda after mitigation actions take place
# mitigate <- function(cull, pens, rs){
#   cull * pens* rs
# }
# 
# #######################################################
# # step 7: sum the current events to get the total top event frequency ----
# # Summarized in Table 2 of the manuscript (Winder et al. 2019)
# 
# threatCalculator <- function(init, barriers) {
#   init * prod(barriers)
# }
# 
# 
# (topEvent <- topEventCalculator(threatCalculator(Threat1_InitialFreq, sapply(Threat1_barriers, eval)), 
#                                 threatCalculator(Threat2_InitialFreq, Threat2_barriers), 
#                                 threatCalculator(Threat3_InitialFreq, Threat3_barriers), 
#                                 threatCalculator(Threat4_InitialFreq, Threat4_barriers)))
# # in other words, the sum of the Threat_topevents - which meets the LOPA users manual instructions
# topEvent_Lambda <-round(((1 - topEvent) + 1), 3)
# message("This is the top event lambda: ", topEvent_Lambda) 
# 
# # does the top event exceed the lambda quartile?
# print(TopEvent_lambda <- lambdaQuartile < 1 + (1-topEvent))
# 
# }
# 
# 
# ##################################################################################################
# # step 8: what about after mitigation? ----
# #postMitigate <- function(topEvent, mitigate) {
# #  topEvent * mitigate
# #}
# 
# #mitigate_cull <- 0.814 # from Hervieux et al. Threat1_barrier1 rationale
# #mitigate_restoration <- 1-(Threat_LambdaEffect[[1]][2] + (0.5*Threat_LambdaEffect[[1]][1])) # 100% of threat 1 barrier2 lambda (restoration), plut 50% of threat1_barrier1 (predation - which interacts with seismic lines)
# #mitigate_penning <- 0.95 # from rough pers coms with Scott McNay
# #postMitigateS <- postMitigate(topEvent, mitigate(mitigate_cull, mitigate_restoration, mitigate_penning)) # combined mitigation/normal scenario
# # values are from the literature, and can be changed # ASSUMTION/DATA
# #message("This is the consequence frequency: ", postMitigateS)
# 
# # Convert these inital frequency to values of lambda:
# #Mitigation_LambdaEffect <- list()
# #Mitigation_LambdaEffect[[1]] <- topEvent * (1 - mitigate_restoration) # linear restoration mitigation value, in Lambda units
# #Mitigation_LambdaEffect[[2]] <- topEvent* (1 - mitigate_cull) # wolf cull mitigation value, in Lambda units
# #Mitigation_LambdaEffect[[3]] <- topEvent * (1 - mitigate_penning) # maternal penning (exclosures) mitigation value, in Lambda units 
# 
# 
# #print(PostMitigate_lambda <- 1 + (1-topEvent) < 1 + (1-postMitigateS))
# #message("This is the consequence lambda: ", (1 + (1-postMitigateS)))
# 
# 
# 
# 
# #######################
# # Step 8b: look at different management scenarios by changing the alternate value to equal 1 (i.e. no effect)
# # un-comment below lines to look at these strategies, and how the postmitigate value changes
# 
# #postMitigateS <- postMitigate(topEvent, mitigate(mitigate_cull, 1, 1)) # this is the wolfcull lever - acting solo
# #postMitigateS <- postMitigate(topEvent, mitigate(1, mitigate_penning, 1)) # this is the maternity pen lever - acting solo # 1.16
# #postMitigateS <- postMitigate(topEvent, mitigate(1, 1, mitigate_restoration)) # this is seismic lines - acting solo
# #postMitigateS <- postMitigate(topEvent, mitigate(1, mitigate_penning, mitigate_restoration)) # maternity penning and linear restoration
# 
# #message("This is the consequence lambda after mitigation scenarios: ", (1 + (1-postMitigateS)))
# 
# #################################################################################################################################
# # Step 9: Print the BRAT table, with values ----
# # this should be the same as Figure 2 in the manuscipt
# # run the below code (up to Step 9b) to make a crude output figure similar to the BRAT diagram
# # currently just for Threats and barriers. Future versions will involve the Hazzard, mitigation, and consequence values
# 
# # install.packages('diagram')
# # library(diagram)
# 
# # # creates an empty plot
# # openplotmat()
# # 
# # pdf("BRAT_ThreatsAndBarriers.pdf")
# # # create the coordinates
# # # I want the boxes arranged in a 8, 2, 4, 6 formation (for Threat names/initial/top values, and one for each barrier)
# # pos <- coordinates(c(8,2,4,6))
# # pos # gives the position of these boxes
# # class(pos)
# # plot(pos, type = 'n', main = "BRAT diagram Threats and barriers", xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "")
# # #text(pos)
# # 
# # # add arrows and segments between positional numbers first
# # # Threat1
# # segmentarrow(from = pos[1,], to = pos[8,], dd = 0.45)
# # # Threat2
# # segmentarrow(from = pos[9, ], to = pos[10, ], dd = 0.45)
# # #Threat3
# # segmentarrow(from = pos[11, ], to = pos[14, ], dd = 0.45)
# # #Threat3
# # segmentarrow(from = pos[15, ], to = pos[20, ], dd = 0.45)
# # 
# # # now draw boxes on top of the arrows
# # my_labels<-c(1:20)
# # my_threats<-c(1, 9, 11, 15)
# # my_names_barriers<-c(1, 2, 3, 4, 5, 6, 7, 1, 1, 2, 3, 1, 2, 3, 4, 5)
# # my_barriers<-c(2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 16, 17, 18, 19, 20)
# # my_text_size = 0.9
# # my_edge_length <- 0.05
# # 
# # 
# # # identify the barrier boxes
# # for (i in 1:length(my_labels)) {
# #   if (i %in% 1:length(my_barriers)) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = ("barrier"), cex = my_text_size, box.col = "white")
# #   }
# # }
# # # identify the threat boxes, and add their values
# # for(i in 1:length(my_labels)) {
# #   if (i %in% my_labels[1]){
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "Threat 1 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
# #       text(x = 0.0275, y = 0.71, Threat1_InitialFreq, cex = my_text_size)
# #       text(x = 0.0275, y = 0.69, "Current frequency", cex = my_text_size)
# #       text(x = 0.0275, y = 0.67, Threat1_topEvent, cex = my_text_size)
# #     } else if (i %in% my_labels[9]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 2 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
# #       text(x = 0.230, y = 0.51, Threat2_InitialFreq, cex = my_text_size)
# #       text(x = 0.230, y = 0.49, "Current frequency", cex = my_text_size)
# #       text(x = 0.230, y = 0.47, Threat2_topevent, cex = my_text_size)
# #       } else if (i %in% my_labels[11]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 3 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
# #       text(x = 0.095, y = 0.32, Threat3_InitialFreq, cex = my_text_size)
# #       text(x = 0.095, y = 0.30, "Current frequency", cex = my_text_size)
# #       text(x = 0.095, y = 0.28, Threat3_topevent, cex = my_text_size)
# #       } else if (i %in% my_labels[15]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 4 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
# #       text(x = 0.050, y = 0.13, Threat4_InitialFreq, cex = my_text_size)
# #       text(x = 0.050, y = 0.11, "Current frequency", cex = my_text_size)
# #       text(x = 0.050, y = 0.09, Threat4_topevent, cex = my_text_size)
# #         }
# # }
# # # identify the barrier boxes, and add their values
# # # remind myself of which position numbers represent barriers:
# # # my_barriers<-c(2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 16, 17, 18, 19, 20)
# # for(i in 1:length(my_labels)) {
# #   #For threat 1
# #   if (i %in% my_labels[2]){
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barrier_1, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][1], cex = my_text_size)
# #   } else if (i %in% my_labels[3]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[2], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][2], cex = my_text_size)
# #   } else if (i %in% my_labels[4]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[3], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][3], cex = my_text_size)
# #   } else if (i %in% my_labels[5]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 4 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[4], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][4], cex = my_text_size)
# #   } else if (i %in% my_labels[6]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 5 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[5], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][5], cex = my_text_size)
# #   } else if (i %in% my_labels[7]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 6 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[6], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][6], cex = my_text_size)
# #   } else if (i %in% my_labels[8]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 7 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[7], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][7], cex = my_text_size)
# #   }
# #   # For threat 2
# #   else if (i %in% my_labels[10]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][9]-0.03, Threat2_barrier_1, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][9]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][9]-0.07, Threat_LambdaEffect[[2]][1], cex = my_text_size)
# #   }
# #   # For threat 3
# #   else if (i %in% my_labels[12]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[1], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][1], cex = my_text_size)
# #   } else if (i %in% my_labels[13]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[2], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][2], cex = my_text_size)
# #   } else if (i %in% my_labels[14]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[2], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][3], cex = my_text_size)
# #   }
# #   # For threat 4
# #   else if (i %in% my_labels[16]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[1], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][1], cex = my_text_size)
# #   } else if (i %in% my_labels[17]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[2], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][2], cex = my_text_size)
# #   } else if (i %in% my_labels[18]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[3], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][3], cex = my_text_size)
# #   } else if (i %in% my_labels[19]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 4 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[4], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][4], cex = my_text_size)
# #   } else if (i %in% my_labels[20]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 5 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[5], cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][5], cex = my_text_size)
# #   }
# # }
# # # make sure to Zoom on the plot to expand the figure enough to see all the numbers clearly
# # dev.off()
# # ############################################################################################
# # 
# # # Hazard, mitigation, and consequence portion of the BRAT diagram
# # 
# # pdf("BRAT_HazardMitigationConsequence.pdf")
# # # creates an empty plot
# # openplotmat()
# # # create the coordinates
# # # I want 5 boxes (1 hazzard, 3 mitigations, 1 consequence) all on the same line
# # pos <- coordinates(c(5))
# # pos # gives the position of these boxes
# # class(pos)
# # plot(pos, type = 'n', main = "BRAT diagram hazzard, mitigation, and consequence", xlim = c(0, 1), ylim = c(0.1, 0.8), ylab = "", xlab = "")
# # #text(pos)
# # # add arrows and segments between positional numbers first
# # # Main line
# # segmentarrow(from = pos[1,], to = pos[5,], dd = 0.45)
# # # now draw boxes on top of the arrows
# # my_labels<-c(1:5)
# # my_hazzard<-c(1)
# # my_mitigation<-c(2, 3, 4)
# # my_consequence<-c(5)
# # my_text_size = 0.9
# # my_edge_length <- 0.05
# # # identify the Hazzard box
# # for(i in 1:length(my_labels)) {
# #   if (i %in% my_labels[1]){
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "HAZZARD \n Target Frequency", cex = my_text_size, box.col = "red")
# #     text(x = pos[i], y = pos[,2][1]-0.03, (1+(1-lambdaQuartile)), cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "Target lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, lambdaQuartile, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.09, "Current Total top event frequency: ", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.11,  topEvent, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.13, "Current Total top event lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.15, (1+(1-topEvent)), cex = my_text_size)
# #   } 
# #   }
# # # identify the mitigation boxes
# # for(i in 1:length(my_labels)) {
# #   if (i %in% my_labels[2]){
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "Mitigation 1 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, "0.95", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[1]], cex = my_text_size)
# #   } else if (i %in% my_labels[3]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Mitigation 2 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, "0.814", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[2]], cex = my_text_size)
# #   } else if (i %in% my_labels[4]) {
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Mitigation 3 \n Frequency", cex = my_text_size, box.col = "white")
# #     text(x = pos[i], y = pos[,2][1]-0.03, "0.95", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[3]], cex = my_text_size)
# #   }
# # }
# # # identify the consequence boxes
# # for(i in 1:length(my_labels)) {
# #   if (i %in% my_labels[5]){
# #     textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "CONSEQUENCE \n  target frequency", cex = my_text_size, box.col = "green")
# #     text(x = pos[i], y = pos[,2][1]-0.03, (1+(1-lambdaQuartile)), cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.05, "Consequence target lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.07, lambdaQuartile, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.09, "Current consequence frequency: ", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.11,  postMitigateS, cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.13, "current consequence top event lambda", cex = my_text_size)
# #     text(x = pos[i], y = pos[,2][1]-0.15, (1+(1-postMitigateS)), cex = my_text_size)
# #   } 
# # }
# # # make sure to Zoom on the plot to expand the figure enough to see all the numbers clearly
# # dev.off()
# # #############################################################################################################################
# # # Step 9b: Or you can just print out each value on its own
# # 
# # # Threat 1
# # print("This is Threat 1")
# # message("Threat 1, Initial Frequency: ", Threat1_InitialFreq)
# # message ("Threat 1, Current top event Frequency: ", Threat1_topEvent)
# # 
# # message ("Threat1, barrier1, frequency: ", Threat1_barrier_1)
# # message ("Threat1, barrier2, frequency: ", Threat1_barriers[2])
# # message ("Threat1, barrier3, frequency: ", Threat1_barriers[3])
# # message ("Threat1, barrier4, frequency: ", Threat1_barriers[4])
# # message ("Threat1, barrier5, frequency: ", Threat1_barriers[5])
# # message ("Threat1, barrier6, frequency: ", Threat1_barriers[6])
# # message ("Threat1, barrier7, frequency: ", Threat1_barriers[7])
# # 
# # message ("Threat1, barriers, lambda list 1 through 7: ", Threat_LambdaEffect[c(1)]) # this goes in the comment box
# # 
# # message ("Threat 1, barrier1, comments, additive predation: ", wolvesOnAdults) # this goes in the comment box
# # message ("Threat 1, barrier1, comments, compensatory predation: ", otherOnAdults) # this goes in the comment box
# # message ("Threat 1, barrier1, comments, effectiveness on adults: ",  effectivenessAdults) # this goes in the comment box
# # 
# # 
# # # Threat 2
# # print("This is Threat 2")
# # message("Threat 2, Initial Frequency: ", Threat2_InitialFreq)
# # message ("Threat 2, Current top event Frequency: ", Threat2_topevent)
# # 
# # message ("Threat2, barrier2, frequency: ", Threat2_barrier_1)
# # 
# # message ("Threat2, barriers, lambda: ", Threat_LambdaEffect[c(2)]) # this goes in the comment box
# # 
# # message ("Threat 2, barrier2, comments, additive predation: ", wolvesOnJuvs) # this goes in the comment box
# # message ("Threat 2, barrier2, comments, compensatory predation: ", otherOnJuvs) # this goes in the comment box
# # message ("Threat 2, barrier2, comments, effectiveness on juveniles: ",  effectivenessJuvs) # this goes in the comment box
# # 
# # #Threat3
# # print ("this is Threat 3")
# # message("Threat 3, Initial Frequency: ", Threat3_InitialFreq)
# # message ("Threat 3, Current top event Frequency: ", Threat3_topevent)
# # 
# # message ("Threat3, barrier1, frequency: ", Threat3_barriers[1])
# # message ("Threat3, barrier2, frequency: ", Threat3_barriers[2])
# # message ("Threat3, barrier3, frequency: ", Threat3_barriers[3])
# # 
# # message ("Threat1, barriers, lambda list 1 through 3: ", Threat_LambdaEffect[c(3)]) # these go in the comment boxes
# # 
# # #Threat4
# # print ("this is Threat 4")
# # message("Threat 4, Initial Frequency: ", Threat4_InitialFreq)
# # message ("Threat 4, Current top event Frequency: ", Threat4_topevent)
# # 
# # message ("Threat4, barrier1, frequency: ", Threat4_barriers[1])
# # message ("Threat4, barrier2, frequency: ", Threat4_barriers[2])
# # message ("Threat4, barrier3, frequency: ", Threat4_barriers[3])
# # message ("Threat4, barrier3, frequency: ", Threat4_barriers[4])
# # message ("Threat4, barrier3, frequency: ", Threat4_barriers[5])
# # 
# # message ("Threat1, barriers, lambda list 1 through 5: ", Threat_LambdaEffect[c(4)]) # these go in the comment boxes
# # 
# # 
# # ## Hazzard (Policy objective)
# # message ("This is the target frequency: ", (1+(1-lambdaQuartile)))
# # message ("This is the target lambda: ", lambdaQuartile) # put this in the risk event red circle
# # message ("This is the Current Total top event frequency: ", topEvent)
# # message ("This is the current Total top event lambda: ", (1+(1-topEvent))) # put this in comments
# # 
# # 
# # ### Mitigation Boxes
# # message ("This is the 'responsive restoration of linear features to reduce access': ", 0.95)
# # message ("This is the 'responsive restoriation of linear features to reduce access': ", Mitigation_LambdaEffect[1]) # put this in the comment box
# # 
# # message ("This is the 'wolf cull': ", 0.814)
# # message ("This is the 'wolf cull': ", Mitigation_LambdaEffect[2]) # put this in the comment box
# # 
# # message ("This is the 'Intensive in situ conservation': ", 0.95)
# # message ("This is the 'Intensive in situ conservation': ", Mitigation_LambdaEffect[3]) # put this in the comment box
# # 
# # #### Consequency Box
# # message ("This is the acceptable consequency frequency: ", (1+(1-lambdaQuartile)))
# # message ("This is the current consequence frequency: ", postMitigateS)
# # message ("This is the current consequency lambda: ", (1+(1-postMitigateS))) # put this in a comment box
# # 
# 
# #################################################################################################################################
# #################################################################################################################################
# # Step 10: Repeat the above, but substitute in different demographic data for different herds ----
# # In winder et al. 2019 we have repeated the above with two different herds:
# 
# 
# # snake-sahtahneh herd (STUDY AREA 3) 
# #N = 360 # population of the Chinchaga herd as of 2008
# #SadF = 0.94 # Adult female survival 
# #recr = 0.072/2 # Juvenile female recruitment 
# #sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
# 
# 
# # and An "averaged" population
# # ASSUMPTION: No one herd demography is accurate for a study area. Instead, we quantify the BRAT analysis at the meta-region.
# #N = (360 +  250)/2 # population of the Chinchaga herd as of 2008
# #SadF = (0.94 + 0.87)/2 # Adult female survival 
# #recr = ((0.072 + 0.139)/2)/2 # Juvenile female recruitment 
# #sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
# 
# # NOVEMBER 26, 2019 UPDATE
# # Currently no Maxhamish herd demographic data, but we are working on getting these from FLNRORD
# # it would also be great to get updated demographic data from all herds if available - the data used in this manuscript to date are
# ## 10 year averaged values from 2008 (i.e. collected between 1998-2008)
# 
# ##################################################################################################################################
# # the results of this work is summarized in Tables 1 and 2 of the current BRAT manuscript (Winder et al. 2019)
# ##################################################################################################################################
# 
# 
# 
# ###################################################################################################################################
# # Other information from previous iterations: ----
# ###################################################################################################################################
# # #################################################################################################################################
# # # Threat 1 initial Frequency calcultion laid out
# # 
# # Threat2_barrier_1
# # 
# # TEF = prod(Initial Frequency * barriers)
# # 0.054 = prod (wolves on adults + other on adults)*1.2285
# # 
# # wolvesOnAdults
# # other on adults = x
# # 
# # wolvesOnAdults
# # wolvesOnJuvs
# # 1-wolfCullBarrier
# # 
# # For juveniles we know that:
# # TEF = prod(initial Frequency * barriers)
# # 
# # effectiveness of wolves on juveniles
# # ## assume this ratio (30.7% effectiveness) applies to adults, AND to other populations
# # 
# # total juvienile predation
# # = other on juvs + wolves on juvs (all in lambda units)
# # 
# # threatBarrier1 = (total juvenile predation/inital frequency) + 1
