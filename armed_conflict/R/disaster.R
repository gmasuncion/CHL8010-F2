library(tidyverse)
library(usethis)

# Reading in the data
dat <- read.csv("armed_conflict/original/disaster.csv")
names(dat)[names(dat) == "Year"] <- "year"

# Filtering and adding dummy variables
dat <- dplyr::filter(dat, (year >= 2000 & year <= 2019) & (Disaster.Type == "Earthquake" | Disaster.Type == "Drought"))
dat_sub <- dat %>% select(year, ISO, Disaster.Type)
dat_sub$Drought <- ifelse(dat_sub$Disaster.Type == "Drought", 1, 0)
dat_sub$Earthquake <- ifelse(dat_sub$Disaster.Type == "Earthquake", 1, 0)

# Grouping the data by country and year
dat_group <- dat_sub %>% group_by(year, ISO) %>% summarize(Drought = max(Drought), Earthquake = max(Earthquake))
