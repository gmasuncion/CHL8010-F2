library(tidyverse)
library(usethis)

# Reading in the data
dat <- read.csv("armed_conflict/original/conflictdata.csv")

# Deriving the binary armed conflict variable
dat <- dat %>% group_by(year,ISO) %>% summarize(conflict = sum(best))
dat$conflict <- ifelse(dat$conflict >= 25, 1, 0)
dat$year <- dat$year + 1
conflict_final <- dat