library(tidyverse)
library(wooldridge)
library(broom)
library(AER)
library(magrittr)
library(stargazer)
library(estimatr)

df <- as_tibble(mroz)

df %>% as.data.frame %>% stargazer(type="text")

?mroz

#Half observations for wages compared to total. Not a problem because we want to look at parameters for population of workers.

#Drop missing wages
df %<>% drop_na(wage)

est.stage1 <- lm(educ ~ motheduc + fatheduc + exper + I(exper^2), data=df)

#FIRST STAGE - F stat is large,so it is strong
linearHypothesis(est.stage1,c("motheduc","fatheduc"))

#SECOND STAGE
est.stage2 <- lm(log(wage) ~ fitted(est.stage1) + exper + I(exper^2), data=df)
linearHypothesis(est.stage2,c("motheduc","fatheduc"))

#IV REG
est.2sls <- ivreg(log(wage) ~ educ + exper + I(exper^2) 
                            | motheduc + fatheduc + exper + I(exper^2), data=df)

#NOW OLS
est.ols <- lm(log(wage) ~ educ + exper + I(exper^2),data=df)

#FINAL ANSWER

stargazer(est.ols,est.stage2,est.2sls, type="text")


#For the most part, the IV estimates do look good. However, parental educ may not meet exogenaity condition.
