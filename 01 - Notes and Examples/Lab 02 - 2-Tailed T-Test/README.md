Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(skimr)
library(broom)
library(wooldridge)

#Data Load
df<- as_tibble(audit)

#One-sided t-test
t.test(df$y,alternative="less")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  df$y
    ## t = -4.2768, df = 240, p-value = 1.369e-05
    ## alternative hypothesis: true mean is less than 0
    ## 95 percent confidence interval:
    ##         -Inf -0.08151529
    ## sample estimates:
    ##  mean of x 
    ## -0.1327801

``` r
#Reject null hyp at 5% significance level.

#Two sided test
t.test(df$b,df$w,alternative="two.sided",paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df$b and df$w
    ## t = -4.2768, df = 240, p-value = 2.739e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1939385 -0.0716217
    ## sample estimates:
    ## mean of the differences 
    ##              -0.1327801

``` r
#Load County Murder Data Set (REGRESSION)
df.county <- as_tibble(countymurders)

#Take a glimples at dataset
glimpse(df.county)
```

    ## Rows: 37,349
    ## Columns: 20
    ## $ arrests     <int> 2, 3, 2, 7, 3, 1, 1, 2, 0, 5, 0, 1, 5, 3, 4, 5, 8, 4, 9, 8~
    ## $ countyid    <int> 1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001~
    ## $ density     <dbl> 54.05000, 53.66000, 53.75000, 53.78000, 53.91000, 54.11000~
    ## $ popul       <int> 32216, 31984, 32036, 32056, 32128, 32248, 32888, 33264, 33~
    ## $ perc1019    <dbl> 20.63000, 20.19000, 19.66000, 19.10000, 18.54000, 18.06000~
    ## $ perc2029    <dbl> 15.28000, 15.55000, 15.73000, 15.88000, 15.92000, 15.87000~
    ## $ percblack   <dbl> 22.33000, 22.07000, 21.80000, 21.53000, 21.26000, 20.96000~
    ## $ percmale    <dbl> 40.25000, 40.36000, 40.42000, 40.47000, 40.51000, 40.45000~
    ## $ rpcincmaint <dbl> 167.670, 167.990, 166.630, 176.530, 166.250, 153.120, 151.~
    ## $ rpcpersinc  <dbl> 8780.80, 8232.80, 8327.61, 8545.55, 8965.16, 9254.02, 9885~
    ## $ rpcunemins  <dbl> 29.160, 43.920, 71.410, 72.220, 40.360, 44.540, 38.350, 35~
    ## $ year        <int> 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989~
    ## $ murders     <int> 2, 1, 3, 7, 2, 2, 4, 1, 0, 3, 1, 1, 1, 1, 1, 5, 7, 4, 6, 7~
    ## $ murdrate    <dbl> 0.6208096, 0.3126563, 0.9364465, 2.1836790, 0.6225100, 0.6~
    ## $ arrestrate  <dbl> 0.6208095, 0.9379690, 0.6242977, 2.1836790, 0.9337650, 0.3~
    ## $ statefips   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ countyfips  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3~
    ## $ execs       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ lpopul      <dbl> 10.38022, 10.37299, 10.37462, 10.37524, 10.37748, 10.38121~
    ## $ execrate    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~

``` r
#Run first Regression
est <- lm(murders ~ execs, data=df.county)
tidy(est)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     6.84     0.242      28.3 3.97e-174
    ## 2 execs          65.5      2.15       30.5 7.44e-202

``` r
#Look at r^2
glance(est)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0243        0.0243  46.6      930. 7.44e-202     1 -196508. 3.93e5 3.93e5
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
#alternative summary of regression
summary(est)
```

    ## 
    ## Call:
    ## lm(formula = murders ~ execs, data = df.county)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -202.23   -6.84   -5.84   -3.84 1937.16 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   6.8382     0.2418   28.28   <2e-16 ***
    ## execs        65.4650     2.1463   30.50   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 46.64 on 37347 degrees of freedom
    ## Multiple R-squared:  0.02431,    Adjusted R-squared:  0.02428 
    ## F-statistic: 930.4 on 1 and 37347 DF,  p-value: < 2.2e-16
