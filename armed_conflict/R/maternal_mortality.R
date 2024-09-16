rm(list=ls())

library(tidyverse)
library(usethis)
dat <- read.csv("armed_conflict/original/maternalmortality.csv")

dat_sub <- dat %>% select(Country.Name, X2000:X2019)
dat_sub <-  dat_sub %>% pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "MatMor")
dat_sub$Year <- as.numeric(sub("^.", "", dat_sub$Year))
