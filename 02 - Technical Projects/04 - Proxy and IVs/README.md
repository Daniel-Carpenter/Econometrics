Econometrics Technical Project
================
Daniel Carpenter
Spring 2019

``` r
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
```

    ## # A tibble: 11 x 5
    ##    term         estimate std.error statistic  p.value
    ##    <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  5.71      0.546       10.5   2.96e-24
    ##  2 educ         0.00913   0.0412       0.221 8.25e- 1
    ##  3 exper        0.0124    0.00325      3.84  1.34e- 4
    ##  4 tenure       0.0109    0.00245      4.46  9.17e- 6
    ##  5 married      0.193     0.0389       4.96  8.25e- 7
    ##  6 south       -0.0822    0.0262      -3.13  1.79e- 3
    ##  7 urban        0.178     0.0270       6.58  7.69e-11
    ##  8 black       -0.134     0.0401      -3.35  8.51e- 4
    ##  9 KWW          0.00393   0.00185      2.12  3.45e- 2
    ## 10 IQ          -0.00197   0.00518     -0.381 7.03e- 1
    ## 11 educ:IQ      0.000384  0.000382     1.00  3.16e- 1

``` r
est2 <- lm(logwage ~ educ + exper + tenure
          + married + south + urban + black
          + IQ + educ*IQ, data=df)
tidy(est2)
```

    ## # A tibble: 10 x 5
    ##    term         estimate std.error statistic  p.value
    ##    <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  5.65      0.546       10.3   8.72e-24
    ##  2 educ         0.0185    0.0411       0.449 6.53e- 1
    ##  3 exper        0.0139    0.00318      4.38  1.34e- 5
    ##  4 tenure       0.0114    0.00244      4.67  3.46e- 6
    ##  5 married      0.201     0.0388       5.17  2.82e- 7
    ##  6 south       -0.0802    0.0263      -3.06  2.31e- 3
    ##  7 urban        0.184     0.0269       6.83  1.49e-11
    ##  8 black       -0.147     0.0397      -3.70  2.33e- 4
    ##  9 IQ          -0.000942  0.00516     -0.182 8.55e- 1
    ## 10 educ:IQ      0.000340  0.000383     0.888 3.75e- 1

``` r
est3 <- lm(logwage ~ educ + exper + tenure
           + married + south + urban + black
           + IQ +KWW + educ*IQ, data=df)
tidy(est3)
```

    ## # A tibble: 11 x 5
    ##    term         estimate std.error statistic  p.value
    ##    <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  5.71      0.546       10.5   2.96e-24
    ##  2 educ         0.00913   0.0412       0.221 8.25e- 1
    ##  3 exper        0.0124    0.00325      3.84  1.34e- 4
    ##  4 tenure       0.0109    0.00245      4.46  9.17e- 6
    ##  5 married      0.193     0.0389       4.96  8.25e- 7
    ##  6 south       -0.0822    0.0262      -3.13  1.79e- 3
    ##  7 urban        0.178     0.0270       6.58  7.69e-11
    ##  8 black       -0.134     0.0401      -3.35  8.51e- 4
    ##  9 IQ          -0.00197   0.00518     -0.381 7.03e- 1
    ## 10 KWW          0.00393   0.00185      2.12  3.45e- 2
    ## 11 educ:IQ      0.000384  0.000382     1.00  3.16e- 1

``` r
######15.C3########

df2 <- as_tibble(card)
?card
est.1 <- lm(IQ ~ nearc4,data=df2)
tidy(est.1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   101.       0.627    160.   0       
    ## 2 nearc4          2.60     0.745      3.48 0.000507

``` r
est.2 <- lm(IQ ~ nearc4 +smsa66 + reg662 + 
              reg663 + reg664 + 
              reg665 + reg666 + 
              reg667 + reg668 + 
              reg669,data=df2)
tidy(est.2)
```

    ## # A tibble: 11 x 5
    ##    term        estimate std.error statistic     p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>       <dbl>
    ##  1 (Intercept)  105.        1.62     64.5   0          
    ##  2 nearc4         0.348     0.814     0.427 0.669      
    ##  3 smsa66         1.09      0.809     1.35  0.178      
    ##  4 reg662         1.10      1.65      0.666 0.505      
    ##  5 reg663        -1.56      1.62     -0.961 0.337      
    ##  6 reg664        -0.543     1.92     -0.283 0.777      
    ##  7 reg665        -8.48      1.67     -5.09  0.000000393
    ##  8 reg666        -7.42      1.97     -3.76  0.000175   
    ##  9 reg667        -8.39      1.83     -4.59  0.00000475 
    ## 10 reg668        -2.92      2.34     -1.25  0.212      
    ## 11 reg669        -2.89      1.80     -1.61  0.108
