Econometrics Technical Project
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(broom)
library(wooldridge)
library(skimr)

###################C3.7#####################
t1 <- as_tibble(discrim)

#RESULTS OF PRPBLCK  & INCOME
#BLACK#######
skim(t1$prpblck)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | t1$prpblck |
| Number of rows                                   | 410        |
| Number of columns                                | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|-----:|-----:|-----:|-----:|:------|
| data          |         1 |             1 | 0.11 | 0.18 |   0 | 0.01 | 0.04 | 0.12 | 0.98 | ▇▁▁▁▁ |

``` r
#INCOME######
skim(t1$income)
```

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | t1$income |
| Number of rows                                   | 410       |
| Number of columns                                | 1         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| numeric                                          | 1         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |    p0 |   p25 |   p50 |   p75 |   p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|------:|------:|------:|------:|-------:|:------|
| data          |         1 |             1 | 47053.78 | 13179.29 | 15919 | 37883 | 46272 | 54981 | 136529 | ▃▇▁▁▁ |

``` r
#FORM MULTI REGRESSION OF psoda IN TERMS OF prpblck & income
MR1 <- lm(psoda ~ prpblck + income, data=t1)
tidy(MR1)
```

    ## # A tibble: 3 x 5
    ##   term          estimate   std.error statistic   p.value
    ##   <chr>            <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept) 0.956      0.0190          50.4  1.00e-174
    ## 2 prpblck     0.115      0.0260           4.42 1.26e-  5
    ## 3 income      0.00000160 0.000000362      4.43 1.22e-  5

``` r
#SIMPLE REGRESSION, psoda IN TERMS OF prpblck
SR1 <- lm(psoda ~ prpblck, data=t1)
tidy(SR1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)   1.04     0.00519    200.   0      
    ## 2 prpblck       0.0649   0.0240       2.71 0.00702

``` r
#####LOG REGRESSION MODEL#####
#ADD VARIABLES
t1 <- t1 %>% mutate(logpsoda = log(psoda))
t1 <- t1 %>% mutate(logincome = log(income))

#REGRESS LOG MODEL (LMR1)
LMR1 <- lm(logpsoda ~ prpblck + logincome, data=t1)
tidy(LMR1)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic    p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)  -0.794     0.179      -4.42 0.0000125 
    ## 2 prpblck       0.122     0.0257      4.72 0.00000324
    ## 3 logincome     0.0765    0.0166      4.61 0.00000543

``` r
#CONSTANT PRICE ELAST
LM3 <- lm(psoda ~ income,data=t1)
tidy(LM3)
```

    ## # A tibble: 2 x 5
    ##   term           estimate   std.error statistic   p.value
    ##   <chr>             <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept) 1.00        0.0163          61.6  6.67e-206
    ## 2 income      0.000000907 0.000000333      2.72 6.77e-  3

``` r
#ADD IN "prppov"
LMR2 <- lm(logpsoda ~ prpblck + logincome + prppov, data=t1)
tidy(LMR2)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic     p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 (Intercept)  -1.46      0.294      -4.98 0.000000940
    ## 2 prpblck       0.0728    0.0307      2.37 0.0181     
    ## 3 logincome     0.137     0.0268      5.12 0.000000480
    ## 4 prppov        0.380     0.133       2.86 0.00440

``` r
#CORRELATION B/W LOG(income) & prppov
cov(t1$prppov,t1$logincome)/var(t1$logincome)
```

    ## [1] NA

``` r
cov(t1$logincome,t1$prppov)/var(t1$prppov)
```

    ## [1] NA

``` r
core_g_ <- lm(income ~ prppov,data=t1)
tidy(core_g_)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   57155.      654.      87.3 1.01e-265
    ## 2 prppov      -141675.     6672.     -21.2 6.89e- 68

``` r
###############END OF C3.7##################


###############7.C8#########################
lt1 <- as_tibble(loanapp)

#Regression | approve on white
lt1_est <- lm(approve ~ white, data=lt1)
tidy(lt1_est)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)    0.708    0.0182      38.8 1.09e-245
    ## 2 white          0.201    0.0198      10.1 1.81e- 23

``` r
#Multivariate regression
lt1_est2 <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data=lt1)
tidy(lt1_est2)
```

    ## # A tibble: 16 x 5
    ##    term        estimate std.error statistic  p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  0.937     0.0527     17.8   1.47e-65
    ##  2 white        0.129     0.0197      6.53  8.44e-11
    ##  3 hrat         0.00183   0.00126     1.45  1.47e- 1
    ##  4 obrat       -0.00543   0.00110    -4.93  8.92e- 7
    ##  5 loanprc     -0.147     0.0375     -3.93  8.92e- 5
    ##  6 unem        -0.00730   0.00320    -2.28  2.26e- 2
    ##  7 male        -0.00414   0.0189     -0.220 8.26e- 1
    ##  8 married      0.0458    0.0163      2.81  5.00e- 3
    ##  9 dep         -0.00683   0.00670    -1.02  3.08e- 1
    ## 10 sch          0.00175   0.0166      0.105 9.16e- 1
    ## 11 cosign       0.00977   0.0411      0.238 8.12e- 1
    ## 12 chist        0.133     0.0193      6.91  6.72e-12
    ## 13 pubrec      -0.242     0.0282     -8.57  2.06e-17
    ## 14 mortlat1    -0.0573    0.0500     -1.14  2.52e- 1
    ## 15 mortlat2    -0.114     0.0670     -1.70  8.97e- 2
    ## 16 vr          -0.0314    0.0140     -2.24  2.52e- 2

``` r
#Regress white on obrat
lt1_est3 <- lm(white ~ obrat,data=lt1)
tidy(lt1_est3)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)  0.993    0.0327       30.4  3.49e-167
    ## 2 obrat       -0.00458  0.000977     -4.69 2.98e-  6

``` r
#Hypothesis test
t.test(lt1$white)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  lt1$white
    ## t = 104.16, df = 1988, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8292362 0.8610605
    ## sample estimates:
    ## mean of x 
    ## 0.8451483

``` r
t.test(lt1$white,lt1$obrat,alternative="two.sided",paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  lt1$white and lt1$obrat
    ## t = -169.32, df = 1988, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -31.90925 -31.17852
    ## sample estimates:
    ## mean of the differences 
    ##               -31.54389
