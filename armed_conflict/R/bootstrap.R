library(here)
library(boot)

finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)

data2017inf <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(InfMor)) 
data2017und5 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(Und5Mor)) 
data2017neo <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(NeoMor)) 

getmeddiff_inf <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$InfMor, sample_data$armedconf, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

getmeddiff_und5 <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Und5Mor, sample_data$armedconf, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

getmeddiff_neo <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$NeoMor, sample_data$armedconf, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

set.seed(2024)
bootout_inf <- boot(data2017inf, statistic = getmeddiff_inf, strata = data2017inf$armedconf, R = 1000)
set.seed(2024)
bootout_und5 <- boot(data2017und5, statistic = getmeddiff_und5, strata = data2017und5$armedconf, R = 1000)
set.seed(2024)
bootout_neo <- boot(data2017neo, statistic = getmeddiff_neo, strata = data2017neo$armedconf, R = 1000)

boot.ci(boot.out = bootout_inf, conf = 0.95, type = c("basic", "perc", "bca"))
boot.ci(boot.out = bootout_und5, conf = 0.95, type = c("basic", "perc", "bca"))
boot.ci(boot.out = bootout_neo, conf = 0.95, type = c("basic", "perc", "bca"))

# It can be seen that none of the intervals contain zero and thus there is
# evidence that there exists a difference in the medians between each group
# for each mortality rate