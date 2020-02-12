
MITIGATE<-function(mitigate_cull, mitigate_restoration, mitigate_penning, topEvent){
# variables
  ## cull - 0.814 # from Hervieux et al. 2014 and BRAT() Threat1_barrier1 rationale
  ## restoration - from BRAT function mitigate_restoration - was 0.957 <- 1-(Threat_LambdaEffect[[1]][2] + (0.5*Threat_LambdaEffect[[1]][1])) # 100% of threat 1 barrier2 lambda (restoration), plut 50% of threat1_barrier1 (predation - which interacts with seismic lines)
  ## penning - 0.95 # from discussions with Scott McNay
  ## topEvent - take this from the BRAT()

##################################################################################################
# step 8: what about after mitigation? ----
postMitigate <- function(topEvent, mitigate) {
  topEvent * prod(mitigate_cull, mitigate_restoration, mitigate_penning)
}

#mitigate_cull <- 0.814 # from Hervieux et al. Threat1_barrier1 rationale
#mitigate_restoration <- 1-(Threat_LambdaEffect[[1]][2] + (0.5*Threat_LambdaEffect[[1]][1])) # 100% of threat 1 barrier2 lambda (restoration), plut 50% of threat1_barrier1 (predation - which interacts with seismic lines)
#mitigate_penning <- 0.95 # from rough pers coms with Scott McNay

postMitigateS <- postMitigate(topEvent, mitigate(mitigate_cull, mitigate_restoration, mitigate_penning)) # combined mitigation/normal scenario
# values are from the literature, and can be changed # ASSUMTION/DATA
message("This is the consequence frequency: ", postMitigateS)

# Convert these inital frequency to values of lambda:
Mitigation_LambdaEffect <- list()
Mitigation_LambdaEffect[[1]] <- topEvent * (1 - mitigate_restoration) # linear restoration mitigation value, in Lambda units
Mitigation_LambdaEffect[[2]] <- topEvent* (1 - mitigate_cull) # wolf cull mitigation value, in Lambda units
Mitigation_LambdaEffect[[3]] <- topEvent * (1 - mitigate_penning) # maternal penning (exclosures) mitigation value, in Lambda units


print(PostMitigate_lambda <- 1 + (1-topEvent) < 1 + (1-postMitigateS))
message("This is the consequence lambda post mitigation: ", (1 + (1-postMitigateS)))
message("With mitigation, are the risk threats to caribou prevented?: ", PostMitigate_lambda > 1 + (1-topEvent))

#######################
# Step 8b: look at different management scenarios by changing the alternate value to equal 1 (i.e. no effect)
# un-comment below lines to look at these strategies, and how the postmitigate value changes

#postMitigateS <- postMitigate(topEvent, mitigate(mitigate_cull, 1, 1)) # this is the wolfcull lever - acting solo
#postMitigateS <- postMitigate(topEvent, mitigate(1, mitigate_penning, 1)) # this is the maternity pen lever - acting solo # 1.16
#postMitigateS <- postMitigate(topEvent, mitigate(1, 1, mitigate_restoration)) # this is seismic lines - acting solo
#postMitigateS <- postMitigate(topEvent, mitigate(1, mitigate_penning, mitigate_restoration)) # maternity penning and linear restoration

#message("This is the consequence lambda after mitigation scenarios: ", (1 + (1-postMitigateS)))
return(PostMitigate_lambda)
}
