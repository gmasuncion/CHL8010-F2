library(tidyverse)
library(usethis)

# Reading in all mortality datasets
matmort <- read.csv("armed_conflict/original/maternalmortality.csv")
infmort <- read.csv("armed_conflict/original/infantmortality.csv")
neomort <- read.csv("armed_conflict/original/neonatalmortality.csv")
und5mort <- read.csv("armed_conflict/original/under5mortality.csv")

# General data cleaning function
mortality_clean = function(x, name){
  x_sub <- x %>% select(Country.Name, X2000:X2019)
  x_sub <-  x_sub %>% pivot_longer(cols = starts_with("X"), names_to = "year", values_to = name)
  x_sub$year <- as.numeric(sub("^.", "", x_sub$year))
  return(x_sub)
}

# Applying the function to all datasets to create new ones
matmort <- mortality_clean(matmort, "MatMor")
infmort <- mortality_clean(infmort, "InfMor")
neomort <- mortality_clean(neomort, "NeoMor")
und5mort <- mortality_clean(und5mort, "Und5Mor")

# Merging the 4 datasets into one
mort_overall <- reduce(list(matmort, infmort, neomort, und5mort), full_join)

# Adding ISO-3 to the new dataset and dropping country
library(countrycode)
mort_overall$ISO <- countrycode(mort_overall$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")
mort_overall <- select(mort_overall, -Country.Name)
