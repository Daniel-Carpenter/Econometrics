##### POOLED - RANDOM = FIXED EFFECTS #####

#POOLED, RANDOM, FIXED EFFECTS
#CODE TO SHOW COMPARISON

library(tidyverse)
library(wooldridge)
library(broom)
library(magrittr)
library(stargazer)
library(clubSandwich)
library(plm)

df <- as_tibble(wagepan)
df %<>% mutate(year=as.factor(year))
df %<>% rename(id = nr)

#?wagepan
pdim(df)

df.within <- df %>% select(id,year,educ,married,union,rur) %>%
  group_by(id) %>%
  summarize(
    mean.edu = mean(educ),
    var.edu = var(educ), #shows if education changes for x person
    mean.marr = mean(married), #shows percentage of data that person is married
    var.marr = var(married),
    mean.union = mean(union),
    var.union = var(union),
    mean.rural = mean(rur),
    var.rural = var(rur)
  )
df.within %>% as.data.frame %>% stargazer(type="text")

#1. no variation in personal education among years, married, union, rural = yes
#2. If positive value for mean variables, then they have once been involved with that activity within the years in this dataset
#3. It is important to understand if the dataset includes anything beyond 0 values. If so, then there is not much point in creating an estimate off the individuals.

#POOLED EFFECT
    est.pols <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + rur + year,
                    data = df, index = c("id","year"), model = "pooling")
    tidy(est.pols)
    #Interpretation of union: IF you are in a union, 
    #then you have 18% greater wage than not being in a uion
    
#RANDOM EFFECTS
    est.re <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + rur + year,
                  data = df, index = c("id","year"), model = "random")
    tidy(est.re)
    
    
    #what are the differences in pooling model and random effect?
        #a. 
    
    #5. The SE's in RE are smaller because there is serial correlation. RE has cluter robust SE (model = random (fix))

#FIXED EFFECTS

    est.fe <- plm(lwage ~ I(exper^2) + married + union + rur + year,
                  data = df, index = c("id","year"), model = "within")
    tidy(est.fe)
    #Takes out random effects, focuses on only the fixed effects
    
    
#COMBINE THE RESULTS FOR COMPARISON: CLUSTER SE's
    clust.po <- coef_test(est.pols, vcov = "CR1", cluster = "individual")
    clust.re <- coef_test(est.re, vcov = "CR1", cluster = "individual")
    clust.fe <- coef_test(est.fe, vcov = "CR1", cluster = "individual")
    
    stargazer(est.pols,est.re,est.fe,se=list(clust.po$SE,clust.re$SE,clust.fe$SE),type="text")
    #INTERPRET UNION:
        #OLS: among all, being in a union leads to an 18% increase in wage
        #RE: "" all else constant, there is an 11% increase in wage from things like ability and what have you
        #FE: "" Unions by themselves give a 7% increase in wage 