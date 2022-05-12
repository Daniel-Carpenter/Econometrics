library(tidyverse)
library(broom)
library(wooldridge)
library(car)
library(magrittr)

#Data
df <- as_tibble(htv)

#CHANGE DUMMIES TO REFERENCED
df %<>% mutate(region = case_when(ne==1 ~ "Northeast",
                                  nc==1 ~ "NorthCentral",
                                  west==1 ~ "West",
                                  south==1 ~ "South")) %>%
  mutate(region = factor(region))

#VIEW COUNT
table(df$region)

df <- df %>% mutate(abil.sq = (abil)^2)

est <- lm(educ ~ motheduc + fatheduc + abil + abil.sq + region,data = df)
tidy(est)
#abil.sq is positive, so upward sloping parabola

#1. Test the hypothesis that abil has a linear effect on educ.
    #H0: abil.sq = 0
    #Reject because abil.sq is greater than 0
    #t-stat is 6.06 >> 2, so significant

    #Reject becuse p-value is very small, p < .05, so reject

#2. Now test that motheduc and fatheduc have equal effects on educ. In other words, test H0 : 1 =
#2;Ha : 1 6= 2. To do this, you will need to obtain se(1 ??? 2). Luckily, R will do this for you with
#the linearHypothesis() function in the car package:
# H0: B1 = B2
# H1: B1 <> B2

linearHypothesis(est, "motheduc = fatheduc")
#Failed to reject null hypothesis 
#NOTES: Need to go back and see critical value location

linearHypothesis(est, matchCoefs(est,"region"))
#H0: region = 0, aka region does not influence level of education

#reject null based on p-value (50%)
