## start again from a clean R session
if (!require("Require")) install.packages("Require")

library(tidyverse)
library(readxl)
library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- "."

setPaths(cachePath = file.path(mainDir, "cache"),
         inputPath = file.path(mainDir, "inputs"),
         modulePath = file.path(mainDir, "modules"),
         outputPath = file.path(mainDir, "outputs"))

getPaths() ## check that this is what you wanted

## Should pull from the current spire the number of students
## per s/c and
if (!dir.exists(file.path(getPaths()$modulePath, "randSC"))) {
  newModule(name = "randSC", path = getPaths()$modulePath)
}

## Resample student population from real spire data
if (!dir.exists(file.path(getPaths()$modulePath, "getSPIRE"))) {
  newModule(name = "getSPIRE", path = getPaths()$modulePath)
}

## generate results from the resampled spire data
if (!dir.exists(file.path(getPaths()$modulePath, "useSPIRE"))) {
  newModule(name = "useSPIRE", path = getPaths()$modulePath)
}


## list the modules to use
simModules <- list("randSC", "getSPIRE", "useSPIRE")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  getSPIRE = list(simulationTimeStep = 1, 
                          .noneventTime = 1 ),
  useSPIRE = list(simulationTimeStep = 1),
  randSC = list(simulationTimeStep = 1)
)


## make a list of directory paths
simPaths <- getPaths()


mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, paths = simPaths)


#events(mySim)


# yes, the simulation design is this stupid
# moduleDiagram(mySim)

#objectDiagram(mySim)

# this actually runs the simulation (once)
a <- Sys.time()
mysim <- spades(mySim)
b <- Sys.time()
b-a

# Run it a bunch
trial <- SpaDES.experiment::experiment(mySim, replicates = 5)
restrial <- dplyr::bind_rows(lapply(trial,`[[`,"splitters"))

restrial %>% filter(substr(row.names(.),1,4) == "In S")
restrial %>% filter(substr(row.names(.),1,4) == "Inte")
restrial %>% filter(substr(row.names(.),1,4) == "Out ")


restrial %>% filter(substr(row.names(.),1,4) == "In S") %>% 
  transmute(mean.sf = mean(splitter.fall), min.sf = min(splitter.fall), 
            max.sf = max(splitter.fall), mean.ss = mean(splitter.spring), 
            min.ss = min(splitter.spring), max.ss = max(splitter.spring)) %>% 
  filter(substr(row.names(.), nchar(row.names(.)),nchar(row.names(.))) == 1)

restrial %>% filter(substr(row.names(.),1,4) == "Out ") %>% 
  transmute(mean.sf = mean(splitter.fall), min.sf = min(splitter.fall), 
            max.sf = max(splitter.fall), mean.ss = mean(splitter.spring), 
            min.ss = min(splitter.spring), max.ss = max(splitter.spring)) %>% 
  filter(substr(row.names(.), nchar(row.names(.)),nchar(row.names(.))) == 1)

restrial %>% filter(substr(row.names(.),1,4) == "Inte") %>% 
  transmute(mean.sf = mean(splitter.fall), min.sf = min(splitter.fall), 
            max.sf = max(splitter.fall), mean.ss = mean(splitter.spring), 
            min.ss = min(splitter.spring), max.ss = max(splitter.spring)) %>% 
  filter(substr(row.names(.), nchar(row.names(.)),nchar(row.names(.))) == 1)

#mysim

respire <- mysim$respire

respire %>% select(`Tuition Residency Group`, 
                   finaid.yn, amount, prob) %>% unique()  %>%  
  group_by(`Tuition Residency Group`) %>% 
  summarize(meanprop = mean(finaid.yn), meanamt = mean(amount), meanprob = mean(prob))

respire %>% select(`Tuition Residency Group`, finaid.yn, amount, prob, 
                   `Course School/College Short`, newid) %>% unique()  %>%  
  group_by(`Tuition Residency Group`, `Course School/College Short`) %>% 
  summarize(meanprop = mean(finaid.yn), meanamt = mean(amount), meanprob = mean(prob)) %>%
  print(n=31)

respire %>% select(`Tuition Residency Group`, finaid.yn, amount) %>% unique()  %>%  
  filter(finaid.yn == 0) %>% head


# Code below to get N students per school by major and (sloppily) N students per school by course
# the second one counts courses with 0 or 1 credits equally with courses with 3 or 4 credits
# 
# respire %>% select(newid, `Primary School/College Short`, `Academic Level EOT`) %>% 
#   filter(`Academic Level EOT` == "Junior" ) %>%
#   group_by(`Primary School/College Short`, newid) %>%
#   summarise(myn = n_distinct()) %>%
#   select(newid , `Primary School/College Short`) %>%
#   group_by(`Primary School/College Short`) %>%
#   summarise(myn =n()/dim(.)[1]) -> permajor
# 
# respire %>% select(`Course School/College Short`) %>% 
#   group_by(`Course School/College Short`) %>%
#   summarise(myn =n()/dim(.)[1]) -> perstudent
# 
# data.frame(permajor,perstudent[1:10,2])
# 
# testspire <- head(respire,60000)
# 




