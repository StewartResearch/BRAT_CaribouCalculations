## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "BRATframework",
  description = "R code describing how to calculate the target frequency for a BRAT critical event given our knowledge of caribou recruitment and survival. This code emulates the
  BowtieXP software use for Bowtie Risk Analysis Tool (BRAT). It is currently set up with Chinchaga herd values as a default.",
  keywords = c("caribou", "Risk analysis", "Bowtie Diagram", "probability of failure", "lambda"),
  authors = c(person("Frances", "Stewart", email = "frances.stewart@canada.ca", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre")),
              person("Richard", "Winder", email = "Richard.winder@canada,ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.9", BRATframework = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "BRATframework.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("N", "numeric", 250, NA, NA, "Population size"),
    defineParameter("SadF", "numeric", 0.87, 0, 1, "Poportion of adult female survival"),
    defineParameter("recr", "numeric", 0.065, 0, 1, "Proportion of juvenile recruitment - divided by two so that it is only for females"),
    defineParameter("sd1", "numeric", 0.1, 0, 1, "standard deviation of an assumed normalized lambda distribution"),
    defineParameter("multiplier1", "numeric", 0.9, 0, 1, "Proportion of risk assigned to the first threat line"),
    defineParameter("pregR", "numeric", 0.9, 0, 1, "Proportion of adult females that are pregant, per year (from maternal penning data; Scott McNay"),
    defineParameter("surv", "numeric", 0.8, 0, 1, "Proportion of calves that live to through their first day (from maternal penning data; Scott McNay)"),
    defineParameter("sexRatio", "numeric", 0.5, 0, 1, "Assumed sex ratio male calves:female calves"),
    defineParameter("wolfCullEffect", "numeric", 0.186, NA, NA, "Effect of wolf culls on lambda - calculated from Hervieux et al. 2014"),
    defineParameter("wolfCullPropOnAdults", "numeric", 0.1, 0, 1, "Proportion of the effect of the wolf cull that affects adults - we assumed most of the wolfcull effect happens on juveniles"),
    defineParameter("mitigate_penning", "numeric", 0.95, NA, NA, "from discussions with Scott McNay - maternal penning data of the Klinse-Za herd in British Columbia (WestMoberly/Scott herds)")
    ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    #expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("topEvent", "Numeric", "outputted top event frequency that will be needed in for the MITIGATE function "),
    createsOutput("mitigate_cull", "Numeric", "outputted from BRAT() - from Hervieux et al. 2014 and BRAT() Threat1_barrier1 rationale"),
    createsOutput("mitigate_restoration", "Numeric", "outputted from BRAT() - from BRAT function mitigate_restoration"),
  )
))

## event types
#   - type `init` is required for initialization

doEvent.BRATframework = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

    # Assertions
      if (class(P(sim)$N) != "numeric") stop("N needs to be numeric")
      if (class(P(sim)$SadF) != "numeric") stop("SadF needs to be numeric")
      if (class(P(sim)$recr) != "numeric") stop("recr needs to be numeric")
      if (class(P(sim)$sd1) != "numeric") stop("sd1 needs to be numeric")
      if (class(P(sim)$multiplier1) != "numeric") stop("multiplier1 needs to be numeric")
      if (class(P(sim)$pregR) != "numeric") stop("pregR needs to be numeric")
      if (class(P(sim)$surv) != "numeric") stop("surv needs to be numeric")
      if (class(P(sim)$sexRatio) != "numeric") stop("sexRatio needs to be numeric")
      if (class(P(sim)$wolfCullEffect) != "numeric") stop("wolfCullEffect needs to be numeric")
      if (class(P(sim)$wolfCullPropOnAdults) != "numeric") stop("wolfCullPropOnAdults needs to be numeric")

      # do stuff for this event
      #sim <- Init(sim)
      Returnvalues <- BRAT(N = P(sim)$N, P(sim)$SadF, P(sim)$recr, P(sim)$sd1,  P(sim)$multiplier1,
                           P(sim)$pregR, P(sim)$surv, P(sim)$sexRatio, P(sim)$wolfCullEffect,
                           P(sim)$wolfCullPropOnAdults)
      sim$topEvent = Returnvalues$topEvent
      sim$mitigate_cul = Returnvalues$mitigate_cul
      sim$mitigate_restoration = Returnvalues$mitigate_restoration

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "BRATframework", "mitigate")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "BRATframework", "plot")
     
    },
     plot = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   # do stuff for this event
    #
    #   #plotFun(sim) # uncomment this, replace with object to plot
    #   # schedule future event(s)
       sim$plot <- PLOT()
    #
    #   # e.g.,
    #   #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "BRATframework", "plot")

      # ! ----- STOP EDITING ----- ! #
     },
    mitigate = {

      # Assertions
      if (class(P(sim)$mitigate_penning) != "numeric") stop("mitigate_penning needs to be numeric")
    #
    #
    #   # ! ----- EDIT BELOW ----- ! #
     sim$mitigate <- MITIGATE(sim$mitigate_cull, sim$mitigate_restoration, P(sim)$mitigate_penning, sim$topEvent)

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "BRATframework", "save")
#
#       # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
