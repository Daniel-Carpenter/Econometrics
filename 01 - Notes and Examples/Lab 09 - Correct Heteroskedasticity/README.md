Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#REVIEW OF CONTENTS:
##1. FILTER OUT ROWS AND COLUMNS
##2. BP TEST      (bptest(est))
##3. WHITE TEST
##4. ROBUST SE's  

library(fredr)
library(tidyverse)
library(broom)
library(wooldridge)
library(car)
library(magrittr)
library(lmtest)
library(estimatr)

df <- as_tibble(gpa3)

#FILTER OUT ROWS
df %<>% filter(spring==1)
glimpse(df)
```

    ## Rows: 366
    ## Columns: 23
    ## $ term     <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2~
    ## $ sat      <int> 920, 780, 810, 1080, 960, 790, 820, 820, 1040, 730, 780, 830,~
    ## $ tothrs   <int> 43, 43, 14, 17, 106, 12, 41, 42, 17, 50, 71, 74, 132, 47, 105~
    ## $ cumgpa   <dbl> 2.04, 2.09, 1.78, 2.00, 2.41, 1.16, 1.87, 2.09, 2.17, 1.90, 2~
    ## $ season   <int> 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0~
    ## $ frstsem  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ crsgpa   <dbl> 2.5077, 2.8839, 2.5600, 2.4764, 2.7414, 2.5918, 2.9289, 2.709~
    ## $ verbmath <dbl> 0.48387, 0.81395, 0.88372, 0.80000, 1.00000, 1.13514, 0.78261~
    ## $ trmgpa   <dbl> 2.25, 1.60, 1.29, 2.73, 2.60, 0.25, 2.00, 1.60, 2.13, 2.50, 2~
    ## $ hssize   <int> 10, 123, 119, 318, 383, 44, 344, 228, 91, 482, 78, 196, 300, ~
    ## $ hsrank   <int> 4, 102, 42, 31, 66, 27, 36, 155, 8, 273, 17, 54, 226, 45, 161~
    ## $ id       <dbl> 22, 35, 36, 156, 246, 264, 326, 410, 412, 428, 452, 477, 509,~
    ## $ spring   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ female   <int> 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0~
    ## $ black    <int> 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1~
    ## $ white    <int> 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0~
    ## $ ctrmgpa  <dbl> 0.75000000, -0.60000002, -0.31000006, 0.73000002, -0.20000005~
    ## $ ctothrs  <int> 12, 15, 14, 17, 15, 12, 16, 12, 17, 16, 12, 12, 12, 12, 12, 1~
    ## $ ccrsgpa  <dbl> -0.138700008, 0.016000032, -0.203400135, -0.038500071, -0.112~
    ## $ ccrspop  <dbl> -62.25000, -73.25000, 72.32996, -320.19995, -92.80002, -661.1~
    ## $ cseason  <int> 1, 1, 1, -1, -1, -1, 0, -1, 0, -1, -1, 0, -1, -1, -1, 0, 0, -~
    ## $ hsperc   <dbl> 40.000000, 82.926826, 35.294117, 9.748427, 17.232376, 61.3636~
    ## $ football <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1~

``` r
?gpa3

#FILTER OUT ALL ELSE COLUMNS (SELECT STATEMENT)
df %>% select(cumgpa,sat,hsperc,tothrs,female,black,white)
```

    ## # A tibble: 366 x 7
    ##    cumgpa   sat hsperc tothrs female black white
    ##     <dbl> <int>  <dbl>  <int>  <int> <int> <int>
    ##  1   2.04   920  40        43      1     0     0
    ##  2   2.09   780  82.9      43      0     0     1
    ##  3   1.78   810  35.3      14      0     0     1
    ##  4   2     1080   9.75     17      0     0     1
    ##  5   2.41   960  17.2     106      0     0     1
    ##  6   1.16   790  61.4      12      0     1     0
    ##  7   1.87   820  10.5      41      1     1     0
    ##  8   2.09   820  68.0      42      0     0     1
    ##  9   2.17  1040   8.79     17      0     0     1
    ## 10   1.90   730  56.6      50      0     1     0
    ## # ... with 356 more rows

``` r
#REGRESS cumpga = 0 + 1sat + 2hsperc + 3tothrs + 4female + 5black + 6white + u

est <- lm(cumgpa ~ sat + hsperc + tothrs 
          + female + black + white,data=df)
tidy(est)
```

    ## # A tibble: 7 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  1.47     0.230        6.40  4.94e-10
    ## 2 sat          0.00114  0.000179     6.39  5.20e-10
    ## 3 hsperc      -0.00857  0.00124     -6.91  2.27e-11
    ## 4 tothrs       0.00250  0.000731     3.43  6.85e- 4
    ## 5 female       0.303    0.0590       5.14  4.50e- 7
    ## 6 black       -0.128    0.147       -0.870 3.85e- 1
    ## 7 white       -0.0587   0.141       -0.416 6.77e- 1

``` r
#BP TEST - low p calue, reject null
bptest(est)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  est
    ## BP = 44.557, df = 6, p-value = 5.732e-08

``` r
#WHITE TEST - pp test fails, A5 fails
bptest(est, ~ fitted(est) + I(fitted(est)^2) )
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  est
    ## BP = 1.2645, df = 2, p-value = 0.5314

``` r
#ROBUST SE LM
est.rob <- lm_robust(cumgpa ~ sat + hsperc + tothrs 
                     + female + black + white,data=df)
tidy(est.rob)
```

    ##          term     estimate    std.error  statistic      p.value      conf.low
    ## 1 (Intercept)  1.470064766 0.2238293185  6.5677936 1.794270e-10  1.0298833856
    ## 2         sat  0.001140728 0.0001924686  5.9268256 7.248729e-09  0.0007622202
    ## 3      hsperc -0.008566358 0.0014236525 -6.0171689 4.379958e-09 -0.0113661039
    ## 4      tothrs  0.002503998 0.0007413439  3.3776471 8.112185e-04  0.0010460757
    ## 5      female  0.303433294 0.0592945229  5.1173916 5.057872e-07  0.1868250448
    ## 6       black -0.128283685 0.1230101308 -1.0428709 2.977098e-01 -0.3701946623
    ## 7       white -0.058721727 0.1152461964 -0.5095329 6.106918e-01 -0.2853641981
    ##      conf.high  df outcome
    ## 1  1.910246147 359  cumgpa
    ## 2  0.001519235 359  cumgpa
    ## 3 -0.005766611 359  cumgpa
    ## 4  0.003961921 359  cumgpa
    ## 5  0.420041544 359  cumgpa
    ## 6  0.113627293 359  cumgpa
    ## 7  0.167920744 359  cumgpa

``` r
summary(est)
```

    ## 
    ## Call:
    ## lm(formula = cumgpa ~ sat + hsperc + tothrs + female + black + 
    ##     white, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.54320 -0.29104 -0.02252  0.28348  1.24872 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.4700648  0.2298031   6.397 4.94e-10 ***
    ## sat          0.0011407  0.0001786   6.389 5.20e-10 ***
    ## hsperc      -0.0085664  0.0012404  -6.906 2.27e-11 ***
    ## tothrs       0.0025040  0.0007310   3.426 0.000685 ***
    ## female       0.3034333  0.0590203   5.141 4.50e-07 ***
    ## black       -0.1282837  0.1473701  -0.870 0.384616    
    ## white       -0.0587217  0.1409896  -0.416 0.677295    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4693 on 359 degrees of freedom
    ## Multiple R-squared:  0.4006, Adjusted R-squared:  0.3905 
    ## F-statistic: 39.98 on 6 and 359 DF,  p-value: < 2.2e-16

``` r
summary(est.rob)
```

    ## 
    ## Call:
    ## lm_robust(formula = cumgpa ~ sat + hsperc + tothrs + female + 
    ##     black + white, data = df)
    ## 
    ## Standard error type:  HC2 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value  Pr(>|t|)   CI Lower  CI Upper  DF
    ## (Intercept)  1.470065  0.2238293  6.5678 1.794e-10  1.0298834  1.910246 359
    ## sat          0.001141  0.0001925  5.9268 7.249e-09  0.0007622  0.001519 359
    ## hsperc      -0.008566  0.0014237 -6.0172 4.380e-09 -0.0113661 -0.005767 359
    ## tothrs       0.002504  0.0007413  3.3776 8.112e-04  0.0010461  0.003962 359
    ## female       0.303433  0.0592945  5.1174 5.058e-07  0.1868250  0.420042 359
    ## black       -0.128284  0.1230101 -1.0429 2.977e-01 -0.3701947  0.113627 359
    ## white       -0.058722  0.1152462 -0.5095 6.107e-01 -0.2853642  0.167921 359
    ## 
    ## Multiple R-squared:  0.4006 ,    Adjusted R-squared:  0.3905 
    ## F-statistic: 39.17 on 6 and 359 DF,  p-value: < 2.2e-16

``` r
#NOTICE BOTH SMALL P VALUES AND SIMLAR (>) SEs

glance(est)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.401         0.391 0.469      40.0 3.41e-37     6  -239.  494.  525.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
linearHypothesis(est.rob, c('sat','hsperc',
                            'tothrs','female',
                            'black','white'))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## sat = 0
    ## hsperc = 0
    ## tothrs = 0
    ## female = 0
    ## black = 0
    ## white = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: cumgpa ~ sat + hsperc + tothrs + female + black + white
    ## 
    ##   Res.Df Df Chisq Pr(>Chisq)    
    ## 1    365                        
    ## 2    359  6   235  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Restricted model (LM test has same conclusion as f test,
#cannor compare #'s for value of F-stat due to different distributions)
restr <- lm(cumgpa ~ 1, data=df)
LMreg <- lm(resid(restr) ~ sat + hsperc + tothrs + female + black + white, data=df)
LM <- nobs(LMreg)*glance(LMreg)$r.squared
pval <- 1-pchisq(LM,6)
```
