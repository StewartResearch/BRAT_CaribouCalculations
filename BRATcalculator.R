# Function to calculate sensitivity of certain components of BRAT
## Used to address reviewer comments
# we are assuming that all values generated and input in BRAT() are correct.
# the hard coded values here are taken directly from the BRAT figure in Winder et al.

BRATcalc<-function(Threat1_InitialFreq, Threat1_barriers,
                   Threat2_InitialFreq, Threat2_barriers, 
                   Threat3_InitialFreq, Threat3_barriers, 
                   Threat4_InitialFreq, Threat4_barriers)
  {
#calculate the lambda quiartile we would like to use.
  lambdaQuartile <- qnorm(0.6, mean = 1, sd1) # mortality quartile is at the 60% mark of this distribution. 
# in other words, what lambda vlaue do herds need to accomplish to obtain sustainability according to ECCC's 2012 40% threshold.
# Put this lambdaQuartile into the Target frequency of the Hazard box
### This is the frequency we need to try and aim above, to prevent lambda being consistently below 1, given a population sd of 0.1
# in this case it is:
print(lambdaQuartile) 
# 1.025
  
  
# Top Event Frequency Function
# calculate the top event frequency, before mitigation actions take place.
  topEventCalculator <- function(...) {
  sum(unlist(list(...)))
  }
   
#Threat calculator
# calculate the total product of each threat line
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
  
