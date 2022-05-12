Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(broom)
library(wooldridge)
library(car)
library(magrittr)

#Data
df <- as_tibble(htv)

#CHANGE DUMMIES TO REFERENCED
df %<>% mutate(region = case_when(ne==1 ~ "Northeast",
                                  nc==1 ~ "NorthCentral",
                                  west==1 ~ "West",
                                  south==1 ~ "South")) %>%
  mutate(region = factor(region))

#VIEW COUNT
table(df$region)
```

    ## 
    ## NorthCentral    Northeast        South         West 
    ##          458          260          304          208

``` r
df <- df %>% mutate(abil.sq = (abil)^2)

est <- lm(educ ~ motheduc + fatheduc + abil + abil.sq + region,data = df)
tidy(est)
```

    ## # A tibble: 8 x 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)       8.23     0.293      28.1   1.85e-134
    ## 2 motheduc          0.189    0.0283      6.69  3.42e- 11
    ## 3 fatheduc          0.107    0.0197      5.45  5.93e-  8
    ## 4 abil              0.400    0.0304     13.2   4.12e- 37
    ## 5 abil.sq           0.0506   0.00832     6.08  1.59e-  9
    ## 6 regionNortheast   0.187    0.137       1.37  1.71e-  1
    ## 7 regionSouth      -0.0143   0.131      -0.109 9.13e-  1
    ## 8 regionWest        0.0756   0.148       0.510 6.10e-  1

``` r
#abil.sq is positive, so upward sloping parabola

#1. Test the hypothesis that abil has a linear effect on educ.
    #H0: abil.sq = 0
    #Reject because abil.sq is greater than 0
    #t-stat is 6.06 >> 2, so significant

    #Reject becuse p-value is very small, p < .05, so reject

#2. Now test that motheduc and fatheduc have equal effects on educ. In other words, test H0 : 1 =
#2;Ha : 1 6= 2. To do this, you will need to obtain se(1 ??? 2). Luckily, R will do this for you with
#the linearHypothesis() function in the car package:
# H0: B1 = B2
# H1: B1 <> B2

linearHypothesis(est, "motheduc = fatheduc")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## motheduc - fatheduc = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: educ ~ motheduc + fatheduc + abil + abil.sq + region
    ## 
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)  
    ## 1   1223 3789.6                             
    ## 2   1222 3777.9  1    11.656 3.7701 0.0524 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Failed to reject null hypothesis 
#NOTES: Need to go back and see critical value location

linearHypothesis(est, matchCoefs(est,"region"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## regionNortheast = 0
    ## regionSouth = 0
    ## regionWest = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: educ ~ motheduc + fatheduc + abil + abil.sq + region
    ## 
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1   1225 3785.2                           
    ## 2   1222 3777.9  3    7.3408 0.7915 0.4987

``` r
#H0: region = 0, aka region does not influence level of education

#reject null based on p-value (50%)
```
