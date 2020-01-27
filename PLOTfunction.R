Plot<-function() {

#################################################################################################################################
# Step 9: Print the BRAT table, with values ----
# this should be the same as Figure 2 in the manuscipt
# run the below code (up to Step 9b) to make a crude output figure similar to the BRAT diagram
# currently just for Threats and barriers. Future versions will involve the Hazzard, mitigation, and consequence values

install.packages('diagram')
library(diagram)

# creates an empty plot
openplotmat()

pdf("BRAT_ThreatsAndBarriers.pdf")
# create the coordinates
# I want the boxes arranged in a 8, 2, 4, 6 formation (for Threat names/initial/top values, and one for each barrier)
pos <- coordinates(c(8,2,4,6))
pos # gives the position of these boxes
class(pos)
plot(pos, type = 'n', main = "BRAT diagram Threats and barriers", xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "")
#text(pos)

# add arrows and segments between positional numbers first
# Threat1
segmentarrow(from = pos[1,], to = pos[8,], dd = 0.45)
# Threat2
segmentarrow(from = pos[9, ], to = pos[10, ], dd = 0.45)
#Threat3
segmentarrow(from = pos[11, ], to = pos[14, ], dd = 0.45)
#Threat3
segmentarrow(from = pos[15, ], to = pos[20, ], dd = 0.45)

# now draw boxes on top of the arrows
my_labels<-c(1:20)
my_threats<-c(1, 9, 11, 15)
my_names_barriers<-c(1, 2, 3, 4, 5, 6, 7, 1, 1, 2, 3, 1, 2, 3, 4, 5)
my_barriers<-c(2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 16, 17, 18, 19, 20)
my_text_size = 0.9
my_edge_length <- 0.05


# identify the barrier boxes
for (i in 1:length(my_labels)) {
  if (i %in% 1:length(my_barriers)) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = ("barrier"), cex = my_text_size, box.col = "white")
  }
}
# identify the threat boxes, and add their values
for(i in 1:length(my_labels)) {
  if (i %in% my_labels[1]){
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "Threat 1 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
    text(x = 0.0275, y = 0.71, Threat1_InitialFreq, cex = my_text_size)
    text(x = 0.0275, y = 0.69, "Current frequency", cex = my_text_size)
    text(x = 0.0275, y = 0.67, Threat1_topEvent, cex = my_text_size)
  } else if (i %in% my_labels[9]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 2 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
    text(x = 0.230, y = 0.51, Threat2_InitialFreq, cex = my_text_size)
    text(x = 0.230, y = 0.49, "Current frequency", cex = my_text_size)
    text(x = 0.230, y = 0.47, Threat2_topevent, cex = my_text_size)
  } else if (i %in% my_labels[11]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 3 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
    text(x = 0.095, y = 0.32, Threat3_InitialFreq, cex = my_text_size)
    text(x = 0.095, y = 0.30, "Current frequency", cex = my_text_size)
    text(x = 0.095, y = 0.28, Threat3_topevent, cex = my_text_size)
  } else if (i %in% my_labels[15]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Threat 4 \n Initial Frequency", cex = my_text_size, box.col = "#0072B2")
    text(x = 0.050, y = 0.13, Threat4_InitialFreq, cex = my_text_size)
    text(x = 0.050, y = 0.11, "Current frequency", cex = my_text_size)
    text(x = 0.050, y = 0.09, Threat4_topevent, cex = my_text_size)
  }
}
# identify the barrier boxes, and add their values
# remind myself of which position numbers represent barriers:
# my_barriers<-c(2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 16, 17, 18, 19, 20)
for(i in 1:length(my_labels)) {
  #For threat 1
  if (i %in% my_labels[2]){
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barrier_1, cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][1], cex = my_text_size)
  } else if (i %in% my_labels[3]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[2], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][2], cex = my_text_size)
  } else if (i %in% my_labels[4]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[3], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][3], cex = my_text_size)
  } else if (i %in% my_labels[5]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 4 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[4], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][4], cex = my_text_size)
  } else if (i %in% my_labels[6]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 5 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[5], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][5], cex = my_text_size)
  } else if (i %in% my_labels[7]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 6 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[6], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][6], cex = my_text_size)
  } else if (i %in% my_labels[8]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 7 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, Threat1_barriers[7], cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Threat_LambdaEffect[[1]][7], cex = my_text_size)
  }
  # For threat 2
  else if (i %in% my_labels[10]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][9]-0.03, Threat2_barrier_1, cex = my_text_size)
    text(x = pos[i], y = pos[,2][9]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][9]-0.07, Threat_LambdaEffect[[2]][1], cex = my_text_size)
  }
  # For threat 3
  else if (i %in% my_labels[12]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[1], cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][1], cex = my_text_size)
  } else if (i %in% my_labels[13]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[2], cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][2], cex = my_text_size)
  } else if (i %in% my_labels[14]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][11]-0.03, Threat3_barriers[2], cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][11]-0.07, Threat_LambdaEffect[[3]][3], cex = my_text_size)
  }
  # For threat 4
  else if (i %in% my_labels[16]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 1 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[1], cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][1], cex = my_text_size)
  } else if (i %in% my_labels[17]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 2 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[2], cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][2], cex = my_text_size)
  } else if (i %in% my_labels[18]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 3 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[3], cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][3], cex = my_text_size)
  } else if (i %in% my_labels[19]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 4 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[4], cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][4], cex = my_text_size)
  } else if (i %in% my_labels[20]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "barrier 5 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][15]-0.03, Threat4_barriers[5], cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.05, "lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][15]-0.07, Threat_LambdaEffect[[4]][5], cex = my_text_size)
  }
}
# make sure to Zoom on the plot to expand the figure enough to see all the numbers clearly
dev.off()
############################################################################################

# Hazard, mitigation, and consequence portion of the BRAT diagram

pdf("BRAT_HazardMitigationConsequence.pdf")
# creates an empty plot
openplotmat()
# create the coordinates
# I want 5 boxes (1 hazzard, 3 mitigations, 1 consequence) all on the same line
pos <- coordinates(c(5))
pos # gives the position of these boxes
class(pos)
plot(pos, type = 'n', main = "BRAT diagram hazzard, mitigation, and consequence", xlim = c(0, 1), ylim = c(0.1, 0.8), ylab = "", xlab = "")
#text(pos)
# add arrows and segments between positional numbers first
# Main line
segmentarrow(from = pos[1,], to = pos[5,], dd = 0.45)
# now draw boxes on top of the arrows
my_labels<-c(1:5)
my_hazzard<-c(1)
my_mitigation<-c(2, 3, 4)
my_consequence<-c(5)
my_text_size = 0.9
my_edge_length <- 0.05
# identify the Hazzard box
for(i in 1:length(my_labels)) {
  if (i %in% my_labels[1]){
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "HAZZARD \n Target Frequency", cex = my_text_size, box.col = "red")
    text(x = pos[i], y = pos[,2][1]-0.03, (1+(1-lambdaQuartile)), cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "Target lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, lambdaQuartile, cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.09, "Current Total top event frequency: ", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.11,  topEvent, cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.13, "Current Total top event lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.15, (1+(1-topEvent)), cex = my_text_size)
  } 
}
# identify the mitigation boxes
for(i in 1:length(my_labels)) {
  if (i %in% my_labels[2]){
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "Mitigation 1 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, "0.95", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[1]], cex = my_text_size)
  } else if (i %in% my_labels[3]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Mitigation 2 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, "0.814", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[2]], cex = my_text_size)
  } else if (i %in% my_labels[4]) {
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length,lab = "Mitigation 3 \n Frequency", cex = my_text_size, box.col = "white")
    text(x = pos[i], y = pos[,2][1]-0.03, "0.95", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "Mitigation lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, Mitigation_LambdaEffect[[3]], cex = my_text_size)
  }
}
# identify the consequence boxes
for(i in 1:length(my_labels)) {
  if (i %in% my_labels[5]){
    textrect(mid = pos[i,], radx = my_edge_length, rady = my_edge_length, lab = "CONSEQUENCE \n  target frequency", cex = my_text_size, box.col = "green")
    text(x = pos[i], y = pos[,2][1]-0.03, (1+(1-lambdaQuartile)), cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.05, "Consequence target lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.07, lambdaQuartile, cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.09, "Current consequence frequency: ", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.11,  postMitigateS, cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.13, "current consequence top event lambda", cex = my_text_size)
    text(x = pos[i], y = pos[,2][1]-0.15, (1+(1-postMitigateS)), cex = my_text_size)
  } 
}
# make sure to Zoom on the plot to expand the figure enough to see all the numbers clearly
dev.off()
#############################################################################################################################
# Step 9b: Or you can just print out each value on its own

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

}
