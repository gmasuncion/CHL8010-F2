library(here)
library(mice)

finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)

midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)