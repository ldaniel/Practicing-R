# devtools::install_github("ldaniel/fgvr")

library(fgvr)

dataset <-  fgvr::loansdefaulters

data <-  fgvr::createTestAndTrainSamples(dataset, "y_loan_defaulter")

data$data.train
data$data.test
data$event.proportion