library(tidyverse)
library(skimr)
library(broom)
library(wooldridge)

#Data Load
df<- as_tibble(audit)

#One-sided t-test
t.test(df$y,alternative="less")

#Reject null hyp at 5% significance level.

#Two sided test
t.test(df$b,df$w,alternative="two.sided",paired=TRUE)

#Load County Murder Data Set (REGRESSION)
df.county <- as_tibble(countymurders)

#Take a glimples at dataset
glimpse(df.county)

#Run first Regression
est <- lm(murders ~ execs, data=df.county)
tidy(est)

#Look at r^2
glance(est)

#alternative summary of regression
summary(est)