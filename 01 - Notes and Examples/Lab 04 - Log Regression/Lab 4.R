library(tidyverse)
library(broom)
library(wooldridge)

df <- as_tibble(hprice1)
glimpse(hprice1)

#ESTIMATE REGRESSION
est <- lm(price ~ sqrft + bdrms, data=df)
tidy(est)
glance(est)
#For  every increase in square footage, you will have to pay an additional $128. 
#For every increase in bedroom, the price increases by $15,200.

mean(est$residuals) #-6.484712e-16, which is close to 0.

#DEFINE VARIABLE, RUN NEW REGRESSION
df <- df %>% mutate(logprice = log(price), sqrftSq = sqrft^2, bdrmSq = bdrms^2)
est <- lm(logprice ~ sqrft + sqrftSq + bdrms + bdrmSq, data=df)
tidy(est)
glance(est)
#The estimates may be smaller because they are in the form of a percentage, rather than dollars.

est <- lm(sqrft ~ sqrftSq + bdrms + bdrmSq, data=df)
df <- df %>% mutate(sqrft.resid = est$residuals)
est <- lm(logprice ~ sqrft.resid, data=df)
tidy(est)

#Frisch Waugh
beta1 <- sum(df$sqrft.resid*df$logprice)/sum(df$sqrft.resid^2)
print(beta1)