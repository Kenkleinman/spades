## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "makeStudents",
  description = "Generates a population of students",
  keywords = "",
  authors = structure(list(list(given = c("Ken"), family = "Kleinman", role = c("aut", "cre"), 
                                email = "kkleinman@umass.edu", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(makeStudents = "0.0.0.001"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "makeStudents.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.1.0)", "ggplot2"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".courseSelectTime", "numeric", 1, NA, NA,
                    "Simulation time at which the first course selection event should occur."),
    defineParameter(".courseSelectInterval", "numeric", 0.5, NA, NA,
                    "Simulation time interval between course selection events."),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    ## above are default, below are new/custom
    defineParameter(".namesSC","character", c("CNS", "ISOM", "Compsci", "SBS", "Engin", "HFA", 
                                              "SPHHS", "NURSING", "EDUC", "Stockbridge"), NA, NA, 
                    "Names of the schools and colleges"),
    ## defineParameter(".nStudentsPerSC","integer", c(1367, 998, 689, 653, 570, 416,261, 249, 118,23), 0, NA, 
    ##                "In order of CNS ISOM Compsci, SBS Engin, HFA, SPHHS NURSING EDUC, Stockbridge"),
    defineParameter(".noneventTime", "numeric", 1, NA, "This is a dummy placeholder for nonevents",
                    "Describes the simulation time at which the first save event should occur.")
  ),
  inputObjects = bindrows(
    #expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA, ...),
     expectsInput("sim$nStudentsPerSC", "integer", "In order of CNS ISOM Compsci, SBS Engin, HFA, SPHHS NURSING EDUC, Stockbridge")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "students", objectClass = "data.frame", desc = "Student ID and S/C"),
    createsOutput(objectName = "semester", objectClass = "data.frame", desc = "course selections per semester")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.makeStudents = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- mS_Init(sim)

      # schedule future event(s)
      
      # After we make the students, we send them off to pick classes
      
      sim <- scheduleEvent(sim, P(sim)$.courseSelectTime, "makeStudents", "courseSelect")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "makeStudents", "plot")
      #sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "makeStudents", "save")
      
      sim <- scheduleEvent(sim, P(sim)$.noneventTime, "makeStudents", "nothing")
      
    },
    nothing = {
      # ! ----- EDIT BELOW ----- ! #

      nothingFun(sim) # does just what it says

            # ! ----- STOP EDITING ----- ! #
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
#      plotFun(sim) # example of a plotting function
      # schedule future event(s)
      
      # e.g.,
#sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "makeStudents", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
# sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "makeStudents", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    courseSelect = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- courseSelectFun(sim)

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$.courseSelectInterval, 
                           moduleName = "makeStudents" , eventType = "courseSelect")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "makeStudents", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
mS_Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  scnames <- P(sim)$.namesSC
  nstudents <- sim$nStudentsPerSC
  
  ## line of code below just makes a data frame with N row, N = total number of rows in nStudentsPerSC
  ## from input and also the name of the School or College, also an input
  
  sim$students <- data.frame(spireid = (1: (sum(nstudents))), sc = rep(scnames, nstudents))

  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
#Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
#sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
#  return(invisible(sim))
#}

### template for plot events
#plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
#  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
#  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
#  return(invisible(sim))
#}

### template for your event1
courseSelectFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  
  # In this simplistic version, every student gets 4 courses with "course rubrics" 0-999
  # Some students may have the same course twice
  # Same courses for students in every school/college
  
#  course1 <- floor(runif(sim$totalNstudents,0,1000))
#  course2 <- floor(runif(sim$totalNstudents,0,1000))
#  course3 <- floor(runif(sim$totalNstudents,0,1000))
#  course4 <- floor(runif(sim$totalNstudents,0,1000))
 
  courses <- assignCourse(sim) 
  
  sim$semester[[as.character(time(sim))]] <- data.frame(courses)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
nothingFun <- function(sim) {
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
#  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
#  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

# ggplotFn <- function(data, ...) {
#   ggplot(data, aes(TheSample)) +
#     geom_histogram(...)
# }

### add additional events as needed by copy/pasting from above
