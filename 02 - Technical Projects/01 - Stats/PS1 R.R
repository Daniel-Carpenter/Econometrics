library(tidyverse)
library(broom)
library(wooldridge)
library(skimr)

##################C1 DATA###################
t1 <- as_tibble(bwght)

#CREATE FEMALE VARIABLE
t1 <- t1 %>% mutate(female=male<1)

#FEMALE DISTRIBUTIOn; n=665, 47.91%
#summary(t1$female)
665/(665+723)

#FEMALES SMOKING DURING PREGNANCY; n=112
#t1 <- t1 %>% filter(female>0,cigs>0)
#summary(t1$female)

#AVG # CIGS SMOKED BY PREGNANT WOMEN avg=12.41
#t1 <- t1 %>% filter(female>0,cigs>0)
skim(t1$cigs)

#AVG FATHER's EDUCATION
skim(t1$fatheduc)

#AVG FAMILY INCOME: $29,030
#SD FAMILY INCOME: $18,740
skim(t1$faminc)


################C3 DATA####################
t2 <- as_tibble(meap01)

#MAX=0%, MIN=100% (RANGE OF MATH04)
max(t2$math4)
min(t2$math4)

#NUMBER OF SCHOOLS WITH PERFECT MATH PASS RATES
#t2 <- t2 %>% filter(math4==100)
skim(t2$math4)
#n=38
# % of total: 2.08%
38/1823

#Schools with 50% pass rates
#t2 <- t2 %>% filter(math4==50)
skim(t2$math4)
#n=17

#AVG MATH VS READING SCORE
skim(t2$math4) #avg=71.91
skim(t2$read4) #avg=60.06

#CORRELATION B/W MATH AND READING SCORES
cov(t2$read4,t2$math4)/var(t2$math4)

#AVG, SD EXPENDITURE PER PUPIL
skim(t2$exppp) 
#avg=$5,194.87
#SD=$1,091.89

#5500 vs 6000
6000/5500 #9.09% increase in exppp
log(6000)-log(5500)
(6000/5500-1)-(log(6000)-log(5500))
