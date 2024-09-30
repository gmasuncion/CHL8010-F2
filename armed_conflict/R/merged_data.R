library(tidyverse)
library(usethis)

# Reading in all previous made datasets
cov_dat <- read.csv("armed_conflict/original/covariates.csv")
mort_overall <- source("armed_conflict/R/maternal_mortality.R")$value
disaster <- source("armed_conflict/R/disaster.R")$value
conflict <- source("armed_conflict/R/conflict.R")$value

# Combining all datasets
primary_dat <- reduce(list(cov_dat, mort_overall, disaster, conflict), left_join, by=c("year"="year","ISO"="ISO"))
primary_dat$Drought[is.na(primary_dat$Drought)] <- 0
primary_dat$Earthquake[is.na(primary_dat$Earthquak)] <- 0
write.csv(primary_dat,file='armed_conflict/original/primary_data.csv',fileEncoding = "UTF-8", row.names =  FALSE)