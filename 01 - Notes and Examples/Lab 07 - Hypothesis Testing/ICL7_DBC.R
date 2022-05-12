#Daniel Carpenter
library(tidyverse)
library(broom)
library(wooldridge)
library(magrittr)

df <- as_tibble(rdchem)
glimpse(df)

#REGRESSIION RDINTENS ON LOGSALES AND PROFMARG
df <- df %>% mutate(logsales = log(sales))
est <- lm(rdintens ~ logsales + profmarg,data=df)
tidy(est)

#CONFIDENCE INTERVAL
confint(est)

#INTERPRETATION
.321
.321/100*10
.321/100*100

#1. Increase RD by 3.2% pp increase
#2. No, the effect is slim.
#3. No, the p value is larger than 0.1.
#4. Yes, it does. Divide the p value in half and we see it is less than p level of significance.
#5. No, it is larger than significance level.
