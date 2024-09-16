rm(list=ls())

library(tidyverse)
library(usethis)

dat <- read.csv("armed_conflict/original/disaster.csv")
dat <- dplyr::filter(dat, (Year >= 2000 & Year <= 2019) & (Disaster.Type == "Earthquake" | Disaster.Type == "Drought"))
dat_sub <- dat %>% select(Year, ISO, Disaster.Type)
dat_sub$Drought <- ifelse(dat_sub$Disaster.Type == "Drought", 1, 0)
dat_sub$Earthquake <- ifelse(dat_sub$Disaster.Type == "Earthquake", 1, 0)

dat_group <- dat_sub %>% group_by(Year, ISO) %>% summarize(Drought = max(Drought), Earthquake = max(Earthquake))
