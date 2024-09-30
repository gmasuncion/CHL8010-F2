library(tidyverse)
library(usethis)

# Reading in the data
dat <- read.csv("armed_conflict/original/conflictdata.csv")

# Deriving the binary armed conflict variable
dat <- dat %>% group_by(year,ISO) %>% summarize(conflict = sum(best))
dat$armedconf <- ifelse(dat$conflict >= 25, 1, 0)
dat$year <- dat$year + 1
dat$conflict[is.na(dat$conflict)] <- 0
dat$armedconf[is.na(dat$armedconf)] <- 0
conflict_final <- dat