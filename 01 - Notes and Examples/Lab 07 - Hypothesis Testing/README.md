Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(broom)
library(wooldridge)
library(magrittr)

df <- as_tibble(rdchem)
glimpse(df)
```

    ## Rows: 32
    ## Columns: 8
    ## $ rd       <dbl> 430.6, 59.0, 23.5, 3.5, 1.7, 8.4, 2.5, 39.9, 1136.0, 1428.0, ~
    ## $ sales    <dbl> 4570.2, 2830.0, 596.8, 133.6, 42.0, 390.0, 93.9, 907.9, 19773~
    ## $ profits  <dbl> 186.9, 467.0, 107.4, -4.3, 8.0, 47.3, 0.9, 77.4, 2563.0, 4154~
    ## $ rdintens <dbl> 9.421906, 2.084806, 3.937668, 2.619760, 4.047619, 2.153846, 2~
    ## $ profmarg <dbl> 4.0895362, 16.5017662, 17.9959793, -3.2185628, 19.0476189, 12~
    ## $ salessq  <dbl> 2.088673e+07, 8.008900e+06, 3.561702e+05, 1.784896e+04, 1.764~
    ## $ lsales   <dbl> 8.427312, 7.948032, 6.391582, 4.894850, 3.737670, 5.966147, 4~
    ## $ lrd      <dbl> 6.0651798, 4.0775375, 3.1570003, 1.2527629, 0.5306283, 2.1282~

``` r
#REGRESSIION RDINTENS ON LOGSALES AND PROFMARG
df <- df %>% mutate(logsales = log(sales))
est <- lm(rdintens ~ logsales + profmarg,data=df)
tidy(est)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)   0.472     1.68       0.282   0.780
    ## 2 logsales      0.321     0.216      1.49    0.147
    ## 3 profmarg      0.0500    0.0458     1.09    0.283

``` r
#CONFIDENCE INTERVAL
confint(est)
```

    ##                   2.5 %    97.5 %
    ## (Intercept) -2.95566542 3.9001733
    ## logsales    -0.11953943 0.7622362
    ## profmarg    -0.04358744 0.1436608

``` r
#INTERPRETATION
.321
```

    ## [1] 0.321

``` r
.321/100*10
```

    ## [1] 0.0321

``` r
.321/100*100
```

    ## [1] 0.321

``` r
#1. Increase RD by 3.2% pp increase
#2. No, the effect is slim.
#3. No, the p value is larger than 0.1.
#4. Yes, it does. Divide the p value in half and we see it is less than p level of significance.
#5. No, it is larger than significance level.
```
