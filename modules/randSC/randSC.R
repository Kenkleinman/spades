## Thisis the file in which we specify the proportion entering each school/college.  
## Currently done with a fixed N of students overall, probably should be done to more 
## closely mimic reality
## Change lines at the bottom to change this

## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "randSC",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(randSC = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "randSC.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.1.0)", "ggplot2"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".reportInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "sim$realspire", objectClass = "data.frame", desc = "Probability students per school in order of CNS ISOM Compsci, SBS Engin, HFA, SPHHS NURSING EDUC, Stockbridge")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "sim$nspire", objectClass = "data.frame", desc = "Number students per school in order of CNS ISOM Compsci, SBS Engin, HFA, SPHHS NURSING EDUC, Stockbridge"),
    createsOutput(objectName = "sim$finaid", objectClass = "data.frame", desc = "Fin aid details"),
    createsOutput(objectName = "sim$n.finaid", objectClass = "data.frame", desc = "Fin aid numbers"),
    createsOutput(objectName = "sim$n.restype", objectClass = "data.frame", desc = "res type gross numbers")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.randSC = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- rSC.Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "randSC", "report")
      #      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "randSC", "save")

    },

    report = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.reportInterval, "randSC", "rSC.report")

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
rSC.Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  ## find the actual N of students in each sc/college (not by year, as of now), and save it in the 
  ## nspire object
  
  ## count vector of   
  ## CICS, CNS, EDUC, ENG, HFA, ISOM, NUR, Other, PHHS, SBS
  
  ## for some reason, sim$nspire is a list with one vector, myn, in it.  Not what I wanted

  # sim$realspire %>% filter(`Academic Career Code` == "UGRD") %>%
  #   select(ID, `Primary School/College Short`) %>% group_by(`Primary School/College Short`, ID) %>%
  #   summarise(myn = n_distinct()) %>%  select(ID, `Primary School/College Short`) %>%
  #   group_by(`Primary School/College Short`) %>% summarise(myn =n()) %>%
  #   select(myn) %>% as.vector() -> sim$nspire
  
  
## Next step: replicate for instate, out, and international, and by year.  This will improve the model
## and speed it up.  Needs something here as well as something in the getspire function
  
  sim$realspire %>%
    select(ID, `Primary School/College Short`, `Academic Level BOT`,
           `Tuition Residency Group`) %>%
    group_by(`Primary School/College Short`, `Academic Level BOT`,
             `Tuition Residency Group`, ID) %>%
    summarise(myn = n_distinct()) %>%
    select(ID, `Primary School/College Short`, `Academic Level BOT`,
           `Tuition Residency Group`) %>%
    group_by(`Primary School/College Short`, `Academic Level BOT`,
             `Tuition Residency Group`) %>%
    summarise(myn =n()) %>%
    pivot_wider(names_from = c(`Academic Level BOT`,`Tuition Residency Group`),
                names_sep = ".",
                values_from = myn) %>%
    mutate(across(where(is.numeric),coalesce,0)) %>%
    as.data.frame() -> sim$nspire
  
  sim$finaid %>%
    summarise(n.instate = sum(instate), 
              n.nonres = sum(nonres), 
              n.parttime = sum(parttime)) %>% as.data.frame() %>% 
              floor() -> sim$n.finaid
  
  ## Note that OoS and International are grouped for finaid purposes in above data set and thus for below count of total students.
  
  sim$nspire %>%
    summarise( across(where(is.numeric),sum)) %>%
    transmute(instate = sum(`Freshman.In State`, `Sophomore.In State`, `Junior.In State`,`Senior.In State` ),
        oos_int = sum(`Freshman.International`, `Sophomore.International`, `Junior.International`,`Senior.International`,
                      `Freshman.Out of State`, `Sophomore.Out of State`, `Junior.Out of State`,`Senior.Out of State` )) %>%
  as.data.frame -> sim$n.restype 
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template 
rSC.report <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  print("N per school/college")
  sim$nspire

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
  

## KK may need more columns later!!!!
  
readRDS("C:/UMass/CLF/spire/spire1819.rds") %>%
    select(`Term`, `ID`, `Academic Career Code`, `Primary School/College Short`,
           `Academic Level BOT`, `Class Session Code`,  `Term Credit Hours`,
           `Tuition Residency Group`, `Course School/College Short`, `Course Subject Code`,
           `Class Session Code`, `Credit Hours`, `Class Number`) %>%
    filter(`Academic Career Code` == "UGRD" & 
           `Academic Level BOT` != "Post-Bacc Undergraduate" &
           `Class Session Code` != "CH1" ) %>%
    as.data.frame() ->  sim$realspire
  
read_xlsx("C:/UMass/CLF/financial aid/fin.xlsx") %>%
    as.data.frame() -> sim$finaid




  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

