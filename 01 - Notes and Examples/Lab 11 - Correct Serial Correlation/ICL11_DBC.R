#KEYS:
#1. LAG OPERATION: REGRESSION THAT PULLS THE LAG OF PERIOD FOR YOU
#2. TEST FOR SERIAL CORRELATION 
    #a. (HYP TEST: if p-value small/t-stat large, then there is serial correlation)
#3 TEST FOR SERIAL CORRELATION WHEN DO NOT HAVE STRICT EXOGENTATING (meaning add other variables in)


library(tidyverse)
library(wooldridge)
library(broom)
library(car)
library(pdfetch)    #used to pull from web
library(magrittr)
library(tsibble)    #used to get time series data


df.ts <- as_tsibble(intdef, key=id(), index=year)
  ggplot(df.ts, aes(year,inf)) + geom_line()
  # ?intdef

#1. LAG OPERATION: REGRESSION THAT PULLS THE LAG OF PERIOD FOR YOU
  est <- lm(i3 ~ inf + lag(inf,1) + lag(inf,2) + def, data=df.ts)
  tidy(est)
  
  #INTERPRETATION:
  #Current inflation is stats. significant, but past years do not play large effect.
  #Past years not significant because: p-value large, t-stats less than 2.

        
#2. TEST FOR SERIAL CORRELATION (HYP TEST: if p-value small/t-stat large, then there is serial correlation)
  
  #a. DROP NA's FROM LAGS -- first two rows undefined because of t-1 and t-2 lags
  residtemp <- c(NA,NA,resid(est)) 
  df.ts %<>% mutate(resids = residtemp)
  
  #b. REGRESSION FOR HYP. TEST
  est.resid <- lm(resids ~ lag(resids), data=df.ts)
  tidy(est.resid)
  
  #INTERPRETATION: 
  #p-value low, so REJECT NULL HYP that there is NOT serial corrlation
          
#3 TEST FOR SERIAL CORRELATION WHEN DO NOT HAVE STRICT EXOGENTATING (meaning add other variables in)

  #a. ADD IN OTHER VARIABLES
  est.resid.endog <- lm(resids ~ lag(resids) + inf + lag(inf,1) + lag(inf,2) + def, data = df.ts)
  tidy(est.resid.endog)
  
  #INTERPRETATION
  #p-value of lag(residuals) still low, so can still reject null(therefore, serial correll exists.)
          