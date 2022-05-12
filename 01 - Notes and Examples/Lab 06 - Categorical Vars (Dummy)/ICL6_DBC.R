#Daniel Carpenter, Natalie
library(tidyverse)
library(broom)
library(wooldridge)
library(magrittr)

df <- as_tibble(affairs)
glimpse(df)

#MALE = 1
df %<>% mutate(male = factor(male), male = fct_recode(male, yes = "1", no = "0"))

#MAKE NUMS NON-BINARY
df %<>% mutate(ratemarr = factor(ratemarr),
               ratemarr = fct_recode(ratemarr, very_happy = "5", happy = "4", average = "3",
                                     unhappy = "2", very_unhappy = "1")) %>%
  mutate(relig = factor(relig),
         relig = fct_recode(relig, very_relig = "5", relig = "4", average = "3",
                            not_relig = "2", not_at_all_relig = "1")) %>%
  mutate(kids = factor(kids), kids = fct_recode(kids, yes = "1", no = "0")) %>%
  mutate(affair = factor(affair), affair = fct_recode(affair, yes = "1", no = "0"))
glimpse(df)

#COUNT OF TABLE ITEMS
table(df$ratemarr)
table(df$relig)
table(df$ratemarr,df$kids)


#REGRESSION
est1 <- lm(naffairs ~ male + yrsmarr + kids + ratemarr, data=df)
tidy(est1)
#When you decrease marital happiness, the numbers of affairs decrease by 2.28
