## Get the real spire data from excel, turn into data frame, save in the environment

library(readxl)

spire1819 <- as.data.frame(read_excel("C:/UMass/CLF/spire/spire1819.xlsx"))

saveRDS(spire1819, file = "C:/UMass/CLF/spire/spire1819.rds")

## testspire <- readRDS("C:/UMass/CLF/spire/spire1819.rds")




