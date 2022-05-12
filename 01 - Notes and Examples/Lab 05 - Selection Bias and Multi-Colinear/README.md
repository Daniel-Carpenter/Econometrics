Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#Group 2:- Daniel Carpenter, Natalie Cook, Nan Jiang 
#install.packages("car", repos='http://cran.us.r-project.org')

#DATA LOAD
library(tidyverse)
library(broom)
library(wooldridge)
library(car)
df <- as_tibble(wage2)

#VIEW
glimpse(df)
```

    ## Rows: 935
    ## Columns: 17
    ## $ wage    <int> 769, 808, 825, 650, 562, 1400, 600, 1081, 1154, 1000, 930, 921~
    ## $ hours   <int> 40, 50, 40, 40, 40, 40, 40, 40, 45, 40, 43, 38, 45, 38, 40, 50~
    ## $ IQ      <int> 93, 119, 108, 96, 74, 116, 91, 114, 111, 95, 132, 102, 125, 11~
    ## $ KWW     <int> 35, 41, 46, 32, 27, 43, 24, 50, 37, 44, 44, 45, 40, 24, 47, 37~
    ## $ educ    <int> 12, 18, 14, 12, 11, 16, 10, 18, 15, 12, 18, 14, 15, 16, 16, 10~
    ## $ exper   <int> 11, 11, 11, 13, 14, 14, 13, 8, 13, 16, 8, 9, 4, 7, 9, 17, 6, 1~
    ## $ tenure  <int> 2, 16, 9, 7, 5, 2, 0, 14, 1, 16, 13, 11, 3, 2, 9, 2, 9, 10, 7,~
    ## $ age     <int> 31, 37, 33, 32, 34, 35, 30, 38, 36, 36, 38, 33, 30, 28, 34, 35~
    ## $ married <int> 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,~
    ## $ black   <int> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ south   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ urban   <int> 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,~
    ## $ sibs    <int> 1, 1, 1, 4, 10, 1, 1, 2, 2, 1, 1, 1, 2, 3, 1, 1, 3, 2, 3, 3, 0~
    ## $ brthord <int> 2, NA, 2, 3, 6, 2, 2, 3, 3, 1, 1, 2, NA, 1, 1, 2, 3, 3, 1, 2, ~
    ## $ meduc   <int> 8, 14, 14, 12, 6, 8, 8, 8, 14, 12, 13, 16, 12, 10, 12, 6, 12, ~
    ## $ feduc   <int> 8, 14, 14, 12, 11, NA, 8, NA, 5, 11, 14, NA, 12, 10, 12, 8, 10~
    ## $ lwage   <dbl> 6.645091, 6.694562, 6.715384, 6.476973, 6.331502, 7.244227, 6.~

``` r
#REGRESSION (IQ ON WAGE) -> S1
est1 <- lm(IQ ~ educ, data=df)
tidy(est1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    53.7      2.62       20.5 3.36e-77
    ## 2 educ            3.53     0.192      18.4 1.16e-64

``` r
#ADD LOG WAGE COLLUMN
df <- df %>% mutate(logwage=log(wage))

#REGRESSION (LOG(WAGE) ON EDUC)
est2 <- lm(logwage ~ educ, data=df)
tidy(est2)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   5.97     0.0814       73.4 0       
    ## 2 educ          0.0598   0.00596      10.0 1.42e-22

``` r
#REGRESSION ON LOG WAGE ON EDUC AND IQ
est3 <- lm(logwage ~ educ + IQ, data=df)
tidy(est3)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)  5.66     0.0962       58.8  7.80e-316
    ## 2 educ         0.0391   0.00684       5.72 1.43e-  8
    ## 3 IQ           0.00586  0.000998      5.88 5.87e-  9

``` r
#TEST IF ~B1 = ^B1 + ^B2~S1 
est2$coefficients["educ"] == est3$coefficients["educ"] +
  est3$coefficients["IQ"]*est1$coefficients["educ"]
```

    ## educ 
    ## TRUE

``` r
est2$coefficients["educ"] #~b1 = .069
```

    ##       educ 
    ## 0.05983921

``` r
est3$coefficients["educ"] #^b1 = .039
```

    ##      educ 
    ## 0.0391199

``` r
#Beta 1 Tilda > Beta 1 Hat
```
