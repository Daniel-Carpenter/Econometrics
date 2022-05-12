library(tidyverse)
library(wooldridge)
library(broom)
library(AER)
library(magrittr)
library(stargazer)

df <- as_tibble(wage2)
df <- df %>% mutate(logwage=log(wage))
?wage2
#Proxy Variable
est <- lm(logwage ~ educ + exper + tenure
             + married + south + urban + black
             + KWW + educ*IQ, data=df)
tidy(est)

est2 <- lm(logwage ~ educ + exper + tenure
          + married + south + urban + black
          + IQ + educ*IQ, data=df)
tidy(est2)

est3 <- lm(logwage ~ educ + exper + tenure
           + married + south + urban + black
           + IQ +KWW + educ*IQ, data=df)
tidy(est3)


######15.C3########

df2 <- as_tibble(card)
?card
est.1 <- lm(IQ ~ nearc4,data=df2)
tidy(est.1)


est.2 <- lm(IQ ~ nearc4 +smsa66 + reg662 + 
              reg663 + reg664 + 
              reg665 + reg666 + 
              reg667 + reg668 + 
              reg669,data=df2)
tidy(est.2)



