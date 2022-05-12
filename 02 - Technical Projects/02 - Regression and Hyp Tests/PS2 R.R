library(tidyverse)
library(broom)
library(wooldridge)
library(skimr)

###################C3.7#####################
t1 <- as_tibble(discrim)

#RESULTS OF PRPBLCK  & INCOME
#BLACK#######
skim(t1$prpblck)
#INCOME######
skim(t1$income)

#FORM MULTI REGRESSION OF psoda IN TERMS OF prpblck & income
MR1 <- lm(psoda ~ prpblck + income, data=t1)
tidy(MR1)

#SIMPLE REGRESSION, psoda IN TERMS OF prpblck
SR1 <- lm(psoda ~ prpblck, data=t1)
tidy(SR1)

#####LOG REGRESSION MODEL#####
#ADD VARIABLES
t1 <- t1 %>% mutate(logpsoda = log(psoda))
t1 <- t1 %>% mutate(logincome = log(income))

#REGRESS LOG MODEL (LMR1)
LMR1 <- lm(logpsoda ~ prpblck + logincome, data=t1)
tidy(LMR1)

#CONSTANT PRICE ELAST
LM3 <- lm(psoda ~ income,data=t1)
tidy(LM3)

#ADD IN "prppov"
LMR2 <- lm(logpsoda ~ prpblck + logincome + prppov, data=t1)
tidy(LMR2)

#CORRELATION B/W LOG(income) & prppov
cov(t1$prppov,t1$logincome)/var(t1$logincome)
cov(t1$logincome,t1$prppov)/var(t1$prppov)
core_g_ <- lm(income ~ prppov,data=t1)
tidy(core_g_)

###############END OF C3.7##################


###############7.C8#########################
lt1 <- as_tibble(loanapp)

#Regression | approve on white
lt1_est <- lm(approve ~ white, data=lt1)
tidy(lt1_est)

#Multivariate regression
lt1_est2 <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data=lt1)
tidy(lt1_est2)

#Regress white on obrat
lt1_est3 <- lm(white ~ obrat,data=lt1)
tidy(lt1_est3)

#Hypothesis test
t.test(lt1$white)
t.test(lt1$white,lt1$obrat,alternative="two.sided",paired=TRUE)
