## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.

## Resamples a SPIRE data set



defineModule(sim, list(
  name = "getSPIRE",
  description = "Resamples a population of students, with the N of students pulled from randSC module",
  keywords = "",
  authors = structure(list(list(given = c("Ken"), family = "K", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(getSPIRE = "0.0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "getSPIRE.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.1.0)"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".reportInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".msInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first make spire event should occur."),
    defineParameter(".msInterval", "numeric", 1, NA, NA,
                    "Describes the simulation time interval between makespire events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("sim$nspire", "data.frame", "In order of CICS, CNS, EDUC, ENG, HFA, ISOM, NUR, Other, PHHS, SBS
"),
  expectsInput("sim$realspire", "data.frame", "A date frame resampled from the real spire data set")
),
outputObjects = bindrows(
    #createsOutput(objectName = NA, objectClass = NA, desc = NA)
    createsOutput("sim$respire", "data.frame", "A resampled  SPIRE dataset")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.getSPIRE = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- gS.Init(sim)

      # schedule future event(s): NONE, says KK, but leave these here Just In Case
      sim <- scheduleEvent(sim, P(sim)$.msInitialTime, "getSPIRE", "make")
      sim <- scheduleEvent(sim, P(sim)$.reportInitialTime, "getSPIRE", "report")
      #sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "getSPIRE", "save")
    },
    make = {
      sim <- gS.make(sim)
    },
    report = {
      sim <- gS.report(sim)
    },
    
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
gS.Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}


gS.make <- function(sim) {

  
  ##sim$nspire should be a vector, but it's a list with a vector in it!!
  
  ## New nspire is a data frame with a column for each year by residency status
  ## need to rewrite makespire() to use it
  
  sim$respire <- makespire( c("CICS", "CNS", "EDUC", "ENG", "HFA", "ISOM", "NUR", "Other", "PHHS", "SBS") 
                            , sim$nspire , sim$realspire, sim)
  
  sim <- scheduleEvent(sim, time(sim) + P(sim)$.msInterval, "useSPIRE", "templateEvent")
  
  
  return(invisible(sim))
}




### template for your event2
gS.report <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test
  
  print("N per school/college")
  sim$respire %>% filter(`Academic Career Code` == "UGRD") %>% 
    select(ID, `Primary School/College Short`) %>% group_by(`Primary School/College Short`, ID) %>%  
    summarise(myn = n_distinct()) %>%  select(ID, `Primary School/College Short`) %>% 
    group_by(`Primary School/College Short`) %>% summarise(myn =n()) -> outsc
  print(outsc)

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
