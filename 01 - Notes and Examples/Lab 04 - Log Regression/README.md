Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(broom)
library(wooldridge)

df <- as_tibble(hprice1)
glimpse(hprice1)
```

    ## Rows: 88
    ## Columns: 10
    ## $ price    <dbl> 300.000, 370.000, 191.000, 195.000, 373.000, 466.275, 332.500~
    ## $ assess   <dbl> 349.1, 351.5, 217.7, 231.8, 319.1, 414.5, 367.8, 300.2, 236.1~
    ## $ bdrms    <int> 4, 3, 3, 3, 4, 5, 3, 3, 3, 3, 4, 5, 3, 3, 3, 4, 4, 3, 3, 4, 3~
    ## $ lotsize  <dbl> 6126, 9903, 5200, 4600, 6095, 8566, 9000, 6210, 6000, 2892, 6~
    ## $ sqrft    <int> 2438, 2076, 1374, 1448, 2514, 2754, 2067, 1731, 1767, 1890, 2~
    ## $ colonial <int> 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1~
    ## $ lprice   <dbl> 5.703783, 5.913503, 5.252274, 5.273000, 5.921578, 6.144775, 5~
    ## $ lassess  <dbl> 5.855359, 5.862210, 5.383118, 5.445875, 5.765504, 6.027073, 5~
    ## $ llotsize <dbl> 8.720297, 9.200593, 8.556414, 8.433811, 8.715224, 9.055556, 9~
    ## $ lsqrft   <dbl> 7.798934, 7.638198, 7.225482, 7.277938, 7.829630, 7.920810, 7~

``` r
#ESTIMATE REGRESSION
est <- lm(price ~ sqrft + bdrms, data=df)
tidy(est)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  -19.3     31.0       -0.622 5.36e- 1
    ## 2 sqrft          0.128    0.0138     9.29  1.39e-14
    ## 3 bdrms         15.2      9.48       1.60  1.13e- 1

``` r
glance(est)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.632         0.623  63.0      73.0 3.57e-19     2  -488.  984.  994.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
#For  every increase in square footage, you will have to pay an additional $128. 
#For every increase in bedroom, the price increases by $15,200.

mean(est$residuals) #-6.484712e-16, which is close to 0.
```

    ## [1] -6.484712e-16

``` r
#DEFINE VARIABLE, RUN NEW REGRESSION
df <- df %>% mutate(logprice = log(price), sqrftSq = sqrft^2, bdrmSq = bdrms^2)
est <- lm(logprice ~ sqrft + sqrftSq + bdrms + bdrmSq, data=df)
tidy(est)
```

    ## # A tibble: 5 x 5
    ##   term         estimate    std.error statistic  p.value
    ##   <chr>           <dbl>        <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  5.07e+ 0 0.325          15.6    2.19e-26
    ## 2 sqrft        3.74e- 4 0.000247        1.51   1.34e- 1
    ## 3 sqrftSq      7.10e-10 0.0000000508    0.0140 9.89e- 1
    ## 4 bdrms       -1.30e- 1 0.145          -0.898  3.72e- 1
    ## 5 bdrmSq       1.99e- 2 0.0178          1.12   2.67e- 1

``` r
glance(est)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.595         0.576 0.198      30.5 1.28e-15     4   20.3 -28.7 -13.8
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
#The estimates may be smaller because they are in the form of a percentage, rather than dollars.

est <- lm(sqrft ~ sqrftSq + bdrms + bdrmSq, data=df)
df <- df %>% mutate(sqrft.resid = est$residuals)
est <- lm(logprice ~ sqrft.resid, data=df)
tidy(est)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept) 5.63      0.0324     174.    2.33e-111
    ## 2 sqrft.resid 0.000374  0.000380     0.985 3.28e-  1

``` r
#Frisch Waugh
beta1 <- sum(df$sqrft.resid*df$logprice)/sum(df$sqrft.resid^2)
print(beta1)
```

    ## [1] 0.0003741526
