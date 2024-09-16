rm(list=ls())

library(tidyverse)
library(usethis)
dat = read.csv("./original/maternalmortality.csv")

dat_sub <- dat %>% select(Country.Name, X2000:X2019)
dat_sub <-  dat_sub %>% pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "MatMor")
dat_sub$Year <- as.numeric(sub("^.", "", dat_sub$Year))

usethis::use_git_config(user.name = "gmasuncion", user.email = "gm.asuncion@hotmail.com")
# to confirm, generate a git situation-report, your user name and email should appear under Git config (global)
usethis::git_sitrep()
usethis::use_git()
usethis::create_github_token()
usethis::use_github()