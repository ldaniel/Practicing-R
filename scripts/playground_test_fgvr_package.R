# devtools::install_github("ldaniel/fgvr")

library(dplyr)
library(fgvr)

path <- "D:/OneDrive/FGV/06_R/Practicing-R/data/"
base <- read.csv(paste(path,"telco_churn.csv", sep = ""), sep = ",", header = T, stringsAsFactors = T)[,-1] 
base <- mutate(base, Churn = ifelse(base$Churn == "YES", as.integer("1"), as.integer("0")))

mydataset <- fgvr::createTestAndTrainSamples(dataset = base, yvar = "Churn", seed = 12345, percentage = 0.7)

mydataset$data.train
mydataset$data.test
mydataset$event.proportion
