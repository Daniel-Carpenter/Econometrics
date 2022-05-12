#1. STARGAZER SUMMARY STATS
#2. DIFFERENT WAY TO ADD EXP VARIABLE I(age^2)
#3. #TEST RELEVANCE OF IV

library(tidyverse)
library(wooldridge)
library(broom)
library(AER)
library(magrittr)
library(stargazer)
library(skimr)

?fertil2
df <- as_tibble(fertil2)

#SUMMARY STATS WITH SKIMR
skim(df)
#SUMMARY STATS WITH STARGAZER
#df %>% as.data.frame %>% stargazer(type="text")


#Question: What do you think is going on when you see varying numbers of observations across the different variables?
    #Probably because premarital birth(1/2)

est.ols <- lm(children ~ educ + age + I(age^2), data=df)
tidy(est.ols)
  #increasing your age by 1 year increseS your children by .332, but at a decreasing rate

#OTHER WAY TO DO TIDY(EST)
stargazer(est.ols, type="text")

#Create dummy if you born before July 1
df %<>% mutate(firsthalf = mnthborn<7)


#TEST RELEVANCE OF IV
est.iv <- ivreg(children ~ educ + age + I(age^2) | firsthalf + age + I(age^2), data=df)
est.iv1 <- ivreg(educ ~ firsthalf,data=df)
stargazer(est.ols,est.iv1,est.iv, type="text")

#Comment on results
  # IV estimates seem to make sense
  # feducation causes education
  # IV SE is much larger than OLS SE
  # Is firstgald actually a good instrument

