library(plm)
library(texreg)

finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)
finaldata['gdp1000_log'] = log(finaldata["gdp1000"])
finaldata['popdens'] = finaldata['popdens']/100

# Example (year as a fixed effect)
mod1 = lm(MatMor ~ -1 + armedconf + gdp1000 + OECD + popdens + urban +
            agedep + male_edu + temp + rainfall1000 + Earthquake + Drought +
            ISO + as.factor(year), 
          data = finaldata)
mod2 = plm(MatMor ~ armedconf + gdp1000 + OECD + popdens + urban + 
             agedep + male_edu + temp + rainfall1000 + Earthquake + Drought,
           index = c("ISO", "year"),
           effect = "twoways",
           model = "within",
           data = finaldata)

screenreg(list(mod1,mod2))

# Week 8 in-class
preds <- as.formula(" ~ armedconf + gdp1000_log + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                  ISO + as.factor(year)")

matmormod <- plm(update.formula(preds, MatMor ~ .), data = finaldata)
un5mormod <- plm(update.formula(preds, Und5Mor ~ .), data = finaldata)
infmormod <- plm(update.formula(preds, InfMor ~ .), data = finaldata)
neomormod <- plm(update.formula(preds, NeoMor ~ .), data = finaldata)

screenreg(list(matmormod,un5mormod, infmormod, neomormod))