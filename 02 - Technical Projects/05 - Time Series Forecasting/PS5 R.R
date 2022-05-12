library(tidyverse)
library(readxl)
library(skimr)
library(broom)
library(car)
library(pdfetch)
library(zoo)
library(lmtest)
library(sandwich)
library(dynlm)
library(estimatr)
library(car)
library(lmtest)
library(magrittr)
library(NHANES)
library(tsibble)
library(wooldridge)
library(tseries)



##### 11.C10 #####

df <- as_tibble(phillips)
#?phillips
  
#Estimate normal phillips and lag
est <- lm(inf  ~ unem,data=df)
est.delta <- lm(cinf  ~ unem,data=df)

tidy(est)
tidy(est.delta)

#Natural rate unem
1.05/0.502 
2.83/-0.518 

#Test for Unit root
adf.test(df$unem)

#Obtain R2 of cunem as explanatory variable
est.delta.2 <- lm(cinf  ~ cunem,data=df)
summary(est.delta)
summary(est.delta.2)



##### 18.C3 #####

df1 <- as_tibble(volat)
?volat

#Estimate AR(3) model on "pcip"
est.pcip <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3,data=df1)
summary(est.pcip)

#Add in 4th lag to above model
est.pcip.2 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + lag(pcip,4),data=df1)
summary(est.pcip.2)


#Estimate AR(3) model on "pcip" (w/pscp)
est.pcip.3 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + pcsp_1 + pcsp_2 + pcsp_3,data=df1)
summary(est.pcip.3)


#Estimate AR(3) model on "pcip" (w/pscp and i3)
est.pcip.4 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + 
                   pcsp_1 + pcsp_2 + pcsp_3 +
                   ci3 + ci3_1 + ci3_2,data=df1)
summary(est.pcip.4)
