library("here")
library("table1")
library(tidyverse)

finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)

# Grouping the data into bins
finaldata <- finaldata |>  
    mutate(year_group = case_when(
    year >= 2000 & year < 2005 ~ "2000-2004",
    year >= 2005 & year < 2010 ~ "2005-2009",
    year >= 2010 & year < 2015 ~ "2010-2014",
    year >= 2015 & year < 2020 ~ "2015-2019")) |>
    group_by(country_name, year_group) |>
    mutate(overall_conf = if_else(any(armedconf == 1), 1, 0)) |>
    ungroup()

# Using factors for dichotomous variables
finaldata$OECD <- factor(finaldata$OECD, 
                            levels = c(0, 1), 
                            labels = c("No", "Yes"))
finaldata$overall_conf <- factor(finaldata$overall_conf, 
                             levels = c(0, 1), 
                             labels = c("No Armed Conflict", "Armed Conflict"))
finaldata$Drought <- factor(finaldata$Drought, 
                            levels = c(0, 1), 
                            labels = c("No Drought", "Drought"))
finaldata$Earthquake <- factor(finaldata$Earthquake, 
                               levels = c(0, 1), 
                               labels = c("No Earthquake", "Earthquake"))

# Re-labelling variables
label(finaldata$conflict) <- "Death From Conflicts"
label(finaldata$Earthquake) <- "Earthquake Status"
label(finaldata$Drought) <- "Drought Status"
label(finaldata$gdp1000) <- "GDP Per 1000"
label(finaldata$popdens) <- "Population Density"
label(finaldata$male_edu) <- "Male Education"
label(finaldata$temp) <- "Mean Annual Temperature"


# Creating the table
table1(~ OECD + male_edu + gdp1000 + popdens + Drought + Earthquake + temp | overall_conf + year_group, 
       data = finaldata, 
       title = "Summary of Earthquake Data by Armed Conflict",
       group.title = "Exposure: Armed Conflict")