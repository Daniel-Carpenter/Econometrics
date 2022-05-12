Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#Daniel Carpenter, Natalie
library(tidyverse)
library(broom)
library(wooldridge)
library(magrittr)

df <- as_tibble(affairs)
glimpse(df)
```

    ## Rows: 601
    ## Columns: 19
    ## $ id       <int> 4, 5, 6, 11, 12, 16, 23, 29, 43, 44, 45, 47, 49, 50, 53, 55, ~
    ## $ male     <int> 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0~
    ## $ age      <dbl> 37, 27, 27, 32, 27, 57, 22, 32, 37, 22, 57, 32, 22, 37, 32, 2~
    ## $ yrsmarr  <dbl> 10.000, 4.000, 1.500, 15.000, 4.000, 15.000, 0.750, 1.500, 15~
    ## $ kids     <int> 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0~
    ## $ relig    <int> 3, 4, 3, 1, 3, 5, 2, 2, 5, 2, 2, 4, 4, 2, 3, 4, 5, 4, 2, 2, 4~
    ## $ educ     <int> 18, 14, 18, 12, 17, 18, 17, 17, 18, 12, 14, 16, 14, 20, 17, 1~
    ## $ occup    <int> 7, 6, 4, 1, 1, 6, 6, 5, 6, 1, 4, 1, 4, 7, 5, 6, 6, 5, 1, 5, 5~
    ## $ ratemarr <int> 4, 4, 4, 4, 5, 5, 3, 5, 2, 3, 4, 2, 5, 2, 2, 4, 4, 5, 5, 4, 4~
    ## $ naffairs <int> 0, 0, 3, 0, 3, 0, 0, 0, 7, 0, 0, 0, 0, 0, 12, 0, 0, 1, 1, 0, ~
    ## $ affair   <int> 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0~
    ## $ vryhap   <int> 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0~
    ## $ hapavg   <int> 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1~
    ## $ avgmarr  <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ unhap    <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0~
    ## $ vryrel   <int> 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0~
    ## $ smerel   <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1~
    ## $ slghtrel <int> 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0~
    ## $ notrel   <int> 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0~

``` r
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
```

    ## Rows: 601
    ## Columns: 19
    ## $ id       <int> 4, 5, 6, 11, 12, 16, 23, 29, 43, 44, 45, 47, 49, 50, 53, 55, ~
    ## $ male     <fct> yes, no, yes, no, no, yes, yes, no, yes, no, yes, no, yes, ye~
    ## $ age      <dbl> 37, 27, 27, 32, 27, 57, 22, 32, 37, 22, 57, 32, 22, 37, 32, 2~
    ## $ yrsmarr  <dbl> 10.000, 4.000, 1.500, 15.000, 4.000, 15.000, 0.750, 1.500, 15~
    ## $ kids     <fct> no, no, no, yes, yes, yes, no, no, yes, no, yes, yes, no, yes~
    ## $ relig    <fct> average, relig, average, not_at_all_relig, average, very_reli~
    ## $ educ     <int> 18, 14, 18, 12, 17, 18, 17, 17, 18, 12, 14, 16, 14, 20, 17, 1~
    ## $ occup    <int> 7, 6, 4, 1, 1, 6, 6, 5, 6, 1, 4, 1, 4, 7, 5, 6, 6, 5, 1, 5, 5~
    ## $ ratemarr <fct> happy, happy, happy, happy, very_happy, very_happy, average, ~
    ## $ naffairs <int> 0, 0, 3, 0, 3, 0, 0, 0, 7, 0, 0, 0, 0, 0, 12, 0, 0, 1, 1, 0, ~
    ## $ affair   <fct> no, no, yes, no, yes, no, no, no, yes, no, no, no, no, no, ye~
    ## $ vryhap   <int> 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0~
    ## $ hapavg   <int> 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1~
    ## $ avgmarr  <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ unhap    <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0~
    ## $ vryrel   <int> 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0~
    ## $ smerel   <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1~
    ## $ slghtrel <int> 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0~
    ## $ notrel   <int> 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0~

``` r
#COUNT OF TABLE ITEMS
table(df$ratemarr)
```

    ## 
    ## very_unhappy      unhappy      average        happy   very_happy 
    ##           16           66           93          194          232

``` r
table(df$relig)
```

    ## 
    ## not_at_all_relig        not_relig          average            relig 
    ##               48              164              129              190 
    ##       very_relig 
    ##               70

``` r
table(df$ratemarr,df$kids)
```

    ##               
    ##                 no yes
    ##   very_unhappy   3  13
    ##   unhappy        8  58
    ##   average       24  69
    ##   happy         40 154
    ##   very_happy    96 136

``` r
#REGRESSION
est1 <- lm(naffairs ~ male + yrsmarr + kids + ratemarr, data=df)
tidy(est1)
```

    ## # A tibble: 8 x 5
    ##   term               estimate std.error statistic  p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)          2.93      0.845      3.47  0.000551
    ## 2 maleyes              0.0828    0.257      0.322 0.747   
    ## 3 yrsmarr              0.0861    0.0285     3.03  0.00259 
    ## 4 kidsyes             -0.212     0.349     -0.607 0.544   
    ## 5 ratemarrunhappy      0.277     0.877      0.316 0.752   
    ## 6 ratemarraverage     -2.14      0.854     -2.50  0.0126  
    ## 7 ratemarrhappy       -2.28      0.821     -2.77  0.00576 
    ## 8 ratemarrvery_happy  -2.68      0.822     -3.26  0.00117

``` r
#When you decrease marital happiness, the numbers of affairs decrease by 2.28
```
