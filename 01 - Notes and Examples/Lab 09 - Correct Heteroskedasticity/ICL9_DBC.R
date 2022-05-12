#REVIEW OF CONTENTS:
##1. FILTER OUT ROWS AND COLUMNS
##2. BP TEST      (bptest(est))
##3. WHITE TEST
##4. ROBUST SE's  

library(fredr)
library(tidyverse)
library(broom)
library(wooldridge)
library(car)
library(magrittr)
library(lmtest)
library(estimatr)

df <- as_tibble(gpa3)

#FILTER OUT ROWS
df %<>% filter(spring==1)
glimpse(df)
?gpa3

#FILTER OUT ALL ELSE COLUMNS (SELECT STATEMENT)
df %>% select(cumgpa,sat,hsperc,tothrs,female,black,white)

#REGRESS cumpga = 0 + 1sat + 2hsperc + 3tothrs + 4female + 5black + 6white + u

est <- lm(cumgpa ~ sat + hsperc + tothrs 
          + female + black + white,data=df)
tidy(est)

#BP TEST - low p calue, reject null
bptest(est)

#WHITE TEST - pp test fails, A5 fails
bptest(est, ~ fitted(est) + I(fitted(est)^2) )

#ROBUST SE LM
est.rob <- lm_robust(cumgpa ~ sat + hsperc + tothrs 
                     + female + black + white,data=df)
tidy(est.rob)

summary(est)
Summary(est.rob)
#NOTICE BOTH SMALL P VALUES AND SIMLAR (>) SEs

glance(est)
linearHypothesis(est.rob, c('sat','hsperc',
                            'tothrs','female',
                            'black','white'))

# Restricted model (LM test has same conclusion as f test,
#cannor compare #'s for value of F-stat due to different distributions)
restr <- lm(cumgpa ~ 1, data=df)
LMreg <- lm(resid(restr) ~ sat + hsperc + tothrs + female + black + white, data=df)
LM <- nobs(LMreg)*glance(LMreg)$r.squared
pval <- 1-pchisq(LM,6)