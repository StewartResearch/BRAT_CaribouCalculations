# function to calculate the values of the riht hand side of the BRAT in Winder et al.

BRAT<-function(N, SadF, recr, sd1,  multiplier1, pregR, sexRatio, wolfCullEffect, wolfCullPropOnAdults){ 
  # variables
  ## N = herd population size (EC 2012)
  ## SadF = adult female survival (EC 2012)
  ## pregR 0.9 - from Scott McNay - roughly 90% of adult females are pregnant each year
  ## sexRatio = 0.5
  
  #variables that we can change:
  ## sd1 - this was initially 0.1 (from Sutherland sensitivity analysis)
  ## multiplier 1 - this was initially 90% (90% of adult female mortality is due to Threat line 1)
  ## wolfCullEffect - this is from Hervieux et al 2014 and could change with more information <- 0.186
  ## wolfCullPropOnAdults - we assumed that the majority of a wolf cull evect ws on juveniles <- 0.1

  
  
  
  # Objectives: ----
  ### Calculate target frequency for the critical event
  ##### Policy says the target frequency should be 40% (Environment and Climate Change Canada 2012 Recovery Strategy) 
  ## - ECCC has indicated landscape change should not exceed 35%, as this produces a 60% probability of herd persistence. 
  ## In other words, this policy threshold is stated at 40% risk of population decline for any specific population.
  # But this 40% is obtained from a normal distribution 
  ## - i.e. it is the area under a normalized curve (because the models in ECCC2012 assume a normal distribution)
  ## representing the lower 40% of the distribution.
  
  ## Overall, we want to determine if a herd is above the 40% threshold (ie in the 60% probability of survival category)
  ## We therefore set 60% as our target threshold to achieve.
  
  ## Through out this script, assumptions are stated as "ASSUMPTION". Search for this word too see them all.
  
  
  ###########################
  ###########################
  # Chinchaga herd (STUDY AREA 1) ----
  ###########################
  
  #########
  # Set the barriers here -- this would be the numbers that go into the BRAT figure
  # you might have to conduct several of the below steps before dpoing this one, if the barriers are not estimated from literature.
  Threat1_barriers <- c(quote(Threat1_barrier_1), 1.4, 0.65, 1.5, 1.0, 0.9, 1.0) # in units of Initial Frequency
  Threat2_barriers <- quote(c(Threat2_barrier_1)) # in units of Initial Frequency # This is derived from data below
  Threat3_barriers <- c(1.00, 1.00, 1.00) # in units of Initial Frequency
  Threat4_barriers <- c(1.05, 1.05, 1.05, 1.00, 1.0)
  
  ###############################################################################
  # Step 1: know (or estimate) some basic information about each study area/herd. ----
  ### average demographic values from Snake and Chinchaga as of 2008 - from downloadable csv online:
  # source: https://open.canada.ca/data/en/dataset/4eb3e825-5b0f-45a3-8b8b-355188d24b71
  
  # Chinchaga
  
  # Step 1: know (or estimate) some basic information about each study area/herd. 
  ### For Chinchaga: data obtained from the ECCC 2011 scientific report
  #N = 250 # population of the Chinchaga herd as of 2008
  #SadF = 0.87 # Adult female survival 
  #recr = 0.13/2 # Juvenile female recruitment (assuming a 50:50 sex ratio) 
  #sd1 <- 0.1 # ASSUMPTION # this value can go as high as 0.3, from the literature.
  
  # invert these numbers to calculate the mortality values:
  MortSadF = (1 - SadF) # part of this will go into the Initial frequency of the threats
  Mortrecr = (1 - recr/2) # part of this will go into the Initial frequency of the threats
  
  
  
  ################################
  # Step 2: State your assumptions ----
  ### In this example, there are 4 threats. 
  ### Three of these threats apply to adult females, and three of these threats apply to juveniles
  ### One threat each applies to only adult females and juveniles
  ##### We need to separate out a portion of the above mortality measures for each of the frequencies 
  ## (whether initial, or current top event) for each threat.
  
  ##### The literature generally says that most of the mortality is due to predation, 
  ## and that the majority of this is on juveniles (Hervieux et al. 2014; Wittmer et al. 2005 a and b). 
  #####
  
  Threat1_multiplier = MortSadF * multiplier1 # ASSUMPTION # I'm assuming that 90% of the variation in SadF is due to predation. This can be changed.
  #Threat2_multiplier = We calculate this amont below, and name it 'Difference_nonpredation', so we do not include it here.
  multiplier2 = (1 - multiplier1)/2
  Threat3_multiplier = MortSadF * multiplier2# ASSUMPTION # we split the remaining 10% of the variation in SadF between the final two threats. We also split the remaining 5% of rec between the final two threats.
  Threat4_multiplier = Threat3_multiplier # ASSUMPTION
  
  # Assumptions about caribou population demongraphics, from published literaure, reports, and pers. comms:
  # These values are used to calculate the current top event frequency, for juveniles
  # pregnancy rate
  # pregR <- 0.9 # DATA/ASSUMPTION # pregnancy rate - from observations in maternal pens. 
  # (Scott McNay pers com, Kinse-Za maternal penning project)
  # also similar to observations in northeastern alberta (McLoughlin et al. 2003; obtained from serum collected during capture)
  # juvenile survival to the fist day of life
  
  #sexRatio<- 0.5 # only half of the animals born will be female 
  surv1stDay <- 0.8 * sexRatio # DATA/ASSUMPTION # juvenile female survival to the first day - from maternal pen observations 
  # (Scott McNay pers com, Klinse-Za maternal penning project)
  # also similar to observations in northeastern alberta (McLoughlin et al. 2003)
  
  mort1stDay <- 1 - (surv1stDay)  
  
  ###########################################################################################################
  # Step 3: calculate a normal distribution, and indicate the 60% value ----
  ### This 60% value comes from Environment and Climate Change Canada's 2012 boreal caribou recovery strategy
  ### indicating the threshold of 35% landscape disturbance = 60% survival probability of herd persistence. 
  ### In other words, we are OK with 40% of the herds dissapearing, on average.
  
  lambdaQuartile <- qnorm(0.6, mean = 1, sd1) # mortality quartile is at the 60% mark of this distribution. 
  # in other words, what lambda vlaue do herds need to accomplish to obtain sustainability according to ECCC's 2012 40% threshold.
  
  # Put this lambdaQuartile into the Target frequency of the Hazard box
  ### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
  # in this case it is:
  print(lambdaQuartile) 
  # 1.025
  
  ##################################################################################
  # Step 4:populating the BRAT framework Initial frequency and Current top events of threats ----
  # Step 4 thorugh 6 are summarized in Table 2 of the manuscript (Winder et al. 2019)
  
  # Determining what vlaues should go into the Initial Frequency of a Threat (Blue boxes), and what values should 
  # go into the barriers to a threat (White boxes) can be tricky and subjective in the BRAT framework. 
  # The values need to come from the best sourced data, and when those data are limited, they  can be hard to determine.
  
  # However, what is important to realize is that the BRAT framework is adaptable and flexible as new information becomes 
  # available. Our manuscript here is showhow, and can be changed as new information comes to light - not only the number 
  # of threats or barriers to a threat, can be changed, but the associations between these items can be changed too.
  
  ####### Current top event frequencies are from known data 
  ## (ie. known mortality or recruitment from Environment Canada 2008; Environment Canada 2012 data)
  ####### Initial event frequencies are things that we need to find out (i.e. calculate)
  ## (ie. what was the frequency of this threat if there had been no landscape changes)
  
  
  # Threat 1 - General predation
  ### Current Top Event Frequency
  # some herds have very low adult female mortality. Thorugh trial and error with Snake and the Averaged herd, we discovered that
  # if MortSadF is less than 0.1 (i.e less than 10% of adult females are dying) the effectiveness of wolf controls was listed as > 100%.
  # This just didnt make any sense. We therefore assume # ASSUMPTION # that 10% represents an ecological threshold of other processes
  # dominating adult female survival. We discovered that if we limit the initial frequency of adult female mortality to only 40%
  # by predation, the effectiveness of wolf controls made sense (i.e. less than 100%) across all study situations. We therefore assume
  # that if less than 10% of females are dying, 4% will be due to wolf predation, and the other 6% will be due to other causes (highlighted
#   # in threats 3 and 4).
   if(MortSadF < 0.10) {
    Threat1_InitialFreq <- MortSadF*0.4
 
   } else { # if not, we back-calculated the required threat value:
    Threat1_InitialFreq <- 0.05549 # ASSUMPTION
#    # This calculation involved back calculating from the wolf mitigation initial frequency (0.814), to the juvenile predation threat frequency,
#     # translating this value into lambda units of wolf predation and compensatory predation, calculating the ratio between these
#   # predation effects, and applying the ratio to adult wolf vs compensatory predation. From there we could substitute in the current
#    # top event frequency (known from Environment Canada data), to get this inital frequency value.
#   # ASSUMPTION: the effectiveness of wolfculls extends from juveniles to adults equally
#   # ASSUMPTION: This is a constant applied to all populations to determine the threat values of adult predation (as long as adult female
#   # mortality is > 10%. See above portion of the IF statement).
#  ### This might be a big assumption! But it is mitigated by the above IF statement.
#   ###### See how we calculated this at the end of the script ('Other Information').
# }

    Threat1_barrier_1 <-  Threat1_multiplier / (Threat1_InitialFreq * prod(unlist(Threat1_barriers[-1])))
    Threat1_topEvent <- Threat1_multiplier # proportion of caribou mortality rate due to predation, rather than total adult female mortality
    Threat1_InitialFreq <- Threat1_topEvent/prod(sapply(Threat1_barriers, eval))
  
  # Threat 2 - Juvenile predation
  ### Initial Frequency
  ##### (i.e. rather all else being equal, how many cows produce a calf a year later if predation is a factor)
  # Initial frequency = Pregnancy Rate*Adult female survival*Calf survival to Day 30
  Threat2_InitialSurv <- pregR * SadF * surv1stDay # proportion of females that produce a femalecalf
  Threat2_InitialFreq <- (1 - (Threat2_InitialSurv)) # proportion of females at the start of a year that result in no female calf a year later
  
  ### Current Top Event Frequency
  ##### Difference between the calculated Initial Frequency, and the Environment Canada reported value 
  #   (i.e. how many female calf deaths from day 1 to a population survey are unaccounted for?)
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
  # step 5: Convert the inital frequency values to values of lambda ----
  
  Threat_LambdaEffect <- list()
  Threat_LambdaEffect[[1]] <- (Threat1_InitialFreq * sapply(Threat1_barriers, eval)) - Threat1_InitialFreq # in Lambda units
  Threat_LambdaEffect[[2]] <- Threat2_InitialFreq * Threat2_barriers - Threat2_InitialFreq # in Lambda units
  Threat_LambdaEffect[[3]] <- Threat3_InitialFreq * Threat3_barriers - Threat3_InitialFreq # in Lambda units
  Threat_LambdaEffect[[4]] <- Threat4_InitialFreq * Threat4_barriers - Threat4_InitialFreq # in Lambda units
  
  # wolfCullEffect <- 0.186 # DATA/ASSUMPTION 
  # i.e. lambda has the potential to increase by 18.6% during a full wolf cull.
  # Hervieux et al. (2014) found lambda increased between 4.6 (within herds) and 14% (between herds) during a 50% wolf cull. 
  # We avereaged these fundings, and standardized by 50% wolf cull to get the 18.6%
  # this assumes a linear relationship with no threshold effects # ASSUMPTION #
  
  #############################################################################################################################
  # Step 5b
  # we now wanted to split this up between the effect of predation by wolves, and the effect of predation as a compensatory effect 
  # (or "other" predators, OR wolves that are not being culled)
  # i.e. this could include wolf immigration and source/sink dynamics -> an important discussion point.
  
  # We wanted to state this in terms of lambda units
  Threat_LambdaEffect[[2]]
  #  So, 0.444 is predator effect on calves -- partition into "non-compensated wolf" == "True effect of wolves on calves" 
  #     and "other" e.g., 0.186 is max possible... partition amongst adults and juvs e.g., 0.9 and 0.1 -->
  #     0.9 * 0.186 on calves and 0.1 * 0.186  on adults
  #  "other predation" for adults = 0.08775 - (0.1 * wolfCullEffect) = 0.07375 in Lambda units
  #     i.e. Threat_LambdaEffect[[1]] - (0.1*wolfcullEffect)
  #  "other predation" for adults = 
  
  # ASSUMPTION # we assumed that the majority of predation happend on juveniles (Hervieux et al. 2014) - 90% of predation happened on juveniles
  # wolfCullPropOnAdults <- 0.1
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
  
  # Double checking out calculations:
  a <- (otherOnAdults + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1] # Lambda units
  #   In InitialEvent units --> (0.277 + Threat2_InitialFreq) / Threat2_InitialFreq
  b <- (otherOnJuvs + Threat2_InitialFreq[1]) / Threat2_InitialFreq[1] # Lambda units
  #   Wolf part In InitialEvent units --> ((0.0186 - 0.07375) + Threat1_InitialFreq) / Threat1_InitialFreq
  d <- ((wolvesOnAdults) + Threat1_InitialFreq[1]) / Threat1_InitialFreq[1] # Lambda units
  #   Wolf part In InitialEvent units --> ((0.444584 - 0.3185) + Threat2_InitialFreq) / Threat2_InitialFreq
  # Wolf part In InitialEvent units --> 
  f <- ((wolvesOnJuvs) + Threat2_InitialFreq) / Threat2_InitialFreq # Lambda units
  
  # This should be the effect on adults in initial units
  Threat1_barrier_1 <- (a - 1)  + (d - 1)  + 1 # in initial units
  # this should be the effect on Juvniles in initial units 
  (b - 1)  + (f - 1)  + 1 # in initial units 
  # Sum of compensatory causes and non-compensatory causes 
  #  of "the predation effect", which is PropMortDay30ToSurvey_predation
  
  # should be the same as equal:
  sapply(Threat1_barriers, eval)[[1]] # :)
  # also, lambda values should equal
  #Threat1_LambdaEffect = wolvesOnAdults + otherOnAdults # check point if desired
  #Threat2_LambdaEffect = wolvesOnAdults + otherOnJuvs # check point if desired
  
  ########################
  # Step 6: Calculate the Current Total Top Event and Current Consequency (i.e. Mitigate) values ----
  # these values can be changed to better reflect data as it becomes available:
  
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
  # step 7: sum the current events to get the total top event frequency ----
  # Summarized in Table 2 of the manuscript (Winder et al. 2019)
  
  threatCalculator <- function(init, barriers) {
    init * prod(barriers)
  }
  
  
  (topEvent <- topEventCalculator(threatCalculator(Threat1_InitialFreq, sapply(Threat1_barriers, eval)), 
                                  threatCalculator(Threat2_InitialFreq, Threat2_barriers), 
                                  threatCalculator(Threat3_InitialFreq, Threat3_barriers), 
                                  threatCalculator(Threat4_InitialFreq, Threat4_barriers)))
  # in other words, the sum of the Threat_topevents - which meets the LOPA users manual instructions
  topEvent_Lambda <-round(((1 - topEvent) + 1), 3)
  message("This is the top event lambda: ", topEvent_Lambda) 
  
  # does the top event exceed the lambda quartile?
  print(TopEvent_lambda <- lambdaQuartile < 1 + (1-topEvent))
   }
}
