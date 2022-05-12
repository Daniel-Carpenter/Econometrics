#1. TEST FOR SERIAL CORRELATIN = bgtest()

library(tidyverse)
library(wooldridge)
library(broom)
library(car)
library(pdfetch)
library(zoo)
library(lmtest)
library(sandwich)
library(dynlm)

df <- as_tibble(phillips)
df.ts <- zoo(df, order.by=df$year)
ggplot(df.ts, aes(year, inf)) + geom_line() + geom_line(data=df.ts, aes(year, unem), color="red")


#RELATIONSHIP BETWEEN UNEMP and INFLATION? 
est <- dynlm(inf ~ unem, data=df.ts)
tidy(est)

#INTERPRETATION:
#Claims positive correlation? Cannot be right based on phillips curve
#unemployment p value good at 10%, bad at 5%. 


#LOOK AT LAG TO SEE IMPACT ON INFLATION, TEST FOR SERIAL CORRELL
df.ts %<>% mutate(resids = resid(est))
lm(resids ~ lag(resids), data=df.ts) %>% tidy
bgtest(est) #test for serial

#INTERPRETATION
#Yes, there is serial correlation becasue p value super low
    
    
#DO NEWEY WEST TO CORRECT FOR SERIAL CORRELATION
coeftest(est) # re-display baseline results
coeftest(est, vcov=NeweyWest)

#INTERPRETATION
#Because there was serial correlation, we now adjusted for it,
#we are now able to see that the p-value increased by 3x previous value, so we are good.
    
    
#LOOK AT DIFFERENCE IN INFLATION TO SEE RELATIONSHIP, AKA Do expectations matter? 
est.diff <- lm(cinf ~ unem, data = df.ts)
tidy(est.diff)

#INTERPRETATION
#Now unemployment and inflation have negative correll
    
    
#NOW DO BG TEST TO TEST FOR LAG
bgtest(est.diff)

#INTERPRETATION
#No longer serial correlation, can see through p-value results

    
#ARE SE's good?    
coeftest(est, vcov=NeweyWest)
coeftest(est.diff, vcov=NeweyWest)

#The SE's decreased
    