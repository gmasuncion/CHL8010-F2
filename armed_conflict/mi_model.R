library(here)
library(mice)

finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)
finaldata['gdp1000_log'] = log(finaldata["gdp1000"])
finaldata['pctpopdens'] = finaldata['popdens']/100

preds <- as.formula(" ~ armedconf + gdp1000_log + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                  ISO + as.factor(year)")

midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)

preds <- as.formula(" ~ armedconf + gdp1000_log + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                  ISO + as.factor(year)")

matmormod_cc <- lm(update.formula(preds, MatMor ~ .), data = finaldata)
un5mormod_cc <- lm(update.formula(preds, Und5Mor ~ .), data = finaldata)
infmormod_cc <- lm(update.formula(preds, InfMor ~ .), data = finaldata)
neomormod_cc <- lm(update.formula(preds, NeoMor ~ .), data = finaldata)

screenreg(list(matmormod_cc,un5mormod_cc, infmormod_cc, neomormod_cc),
        override.coef = list(matmormod_cc$armedconf,un5mormod_cc$armedconf, 
                             infmormod_cc$armedconf, neomormod_cc$armedconf))

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "MatMor", "InfMor", "NeoMor", "Und5Mor", "gdp1000_log", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "MatMor", "InfMor", "NeoMor", "Und5Mor", "gdp1000_log", "pctpopdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)

matmormod_mi <- pool(with(mice.multi.out, lm(MatMor ~ armedconf + gdp1000_log + OECD + pctpopdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                                   as.factor(ISOnum) + as.factor(year))))

un5mormod_mi <- pool(with(mice.multi.out, lm(Und5Mor ~ armedconf + gdp1000_log + OECD + pctpopdens + urban + 
                                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                                  as.factor(ISOnum) + as.factor(year))))

infmormod_mi <- pool(with(mice.multi.out, lm(InfMor ~ armedconf + gdp1000_log + OECD + pctpopdens + urban + 
                                               agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                                               as.factor(ISOnum) + as.factor(year))))

neomormod_mi <- pool(with(mice.multi.out, lm(NeoMor ~ armedconf + gdp1000_log + OECD + pctpopdens + urban + 
                                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                                  as.factor(ISOnum) + as.factor(year))))

screenreg(list(matmormod_cc,un5mormod_cc, infmormod_cc, neomormod_cc,
             matmormod_mi,un5mormod_mi, infmormod_mi, neomormod_mi),
          override.coef = list(matmormod_cc$armedconf,un5mormod_cc$armedconf, 
                             infmormod_cc$armedconf, neomormod_cc$armedconf,
                             matmormod_mi$armedconf,un5mormod_mi$armedconf, 
                             infmormod_mi$armedconf, neomormod_mi$armedconf),
          custom.model.names = c("MAT CC", "UND5 CC", "INF CC", "NEO CC",
                                 "MAT MI", "UND5 MI", "INF MI", "NEO MI")
          )
