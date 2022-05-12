Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#SUMMARY
#1. Filter, Select
#2. Drop NAs = drop_na()
#3. Unweighted vs Weighted means
#4. Read in CSV from web link

library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(magrittr)
library(NHANES)
library(estimatr)
library(skimr)

df <- as_tibble(NHANESraw)
?NHANESraw

#FILTER ONLY 2019_10 and Age = 19
df %<>% filter(as.character(SurveyYr)=='2009_10' & Age>=19)

#SELECT #SELECT VARIABLES
df %<>% select(WTINT2YR,BMI, SleepHrsNight, Age, Education, Gender, Race1)

#DROP NAs
df %<>% drop_na(BMI, SleepHrsNight, Age, Education, Gender, Race1)
skim(df)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | df   |
| Number of rows                                   | 5971 |
| Number of columns                                | 7    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| factor                                           | 3    |
| numeric                                          | 4    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                |
|:--------------|----------:|--------------:|:--------|---------:|:------------------------------------------|
| Education     |         0 |             1 | FALSE   |        5 | Som: 1676, Hig: 1374, Col: 1215, 9 -: 965 |
| Gender        |         0 |             1 | FALSE   |        2 | fem: 3091, mal: 2880                      |
| Race1         |         0 |             1 | FALSE   |        5 | Whi: 2857, Mex: 1092, Bla: 1082, His: 608 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |      p0 |      p25 |      p50 |      p75 |      p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|--------:|---------:|---------:|---------:|----------:|:------|
| WTINT2YR      |         0 |             1 | 35472.53 | 26194.96 | 4084.48 | 17566.90 | 24930.46 | 52723.34 | 153810.26 | ▇▂▂▁▁ |
| BMI           |         0 |             1 |    29.17 |     6.85 |   13.18 |    24.47 |    28.13 |    32.59 |     84.87 | ▇▇▁▁▁ |
| SleepHrsNight |         0 |             1 |     6.83 |     1.44 |    2.00 |     6.00 |     7.00 |     8.00 |     12.00 | ▁▅▇▁▁ |
| Age           |         0 |             1 |    49.35 |    17.79 |   20.00 |    34.00 |    49.00 |    64.00 |     80.00 | ▇▇▇▆▆ |

``` r
#MEAN BMI IN SAMPLE (UNWEIGHTED,WEIGHTED)
mean(df$BMI)
```

    ## [1] 29.16829

``` r
weighted.mean(df$BMI, w=df$WTINT2YR)
```

    ## [1] 28.7547

``` r
#UNWEIGHTED REGRESSION
est.unweighted <- lm(BMI ~ Gender + Race1 + SleepHrsNight, data=df)
tidy(est.unweighted)
```

    ## # A tibble: 7 x 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     32.7      0.455      71.9  0       
    ## 2 Gendermale      -0.795    0.175      -4.54 5.78e- 6
    ## 3 Race1Hispanic   -1.77     0.343      -5.16 2.53e- 7
    ## 4 Race1Mexican    -0.976    0.291      -3.35 8.08e- 4
    ## 5 Race1White      -2.01     0.243      -8.25 1.93e-16
    ## 6 Race1Other      -4.27     0.424     -10.1  1.25e-23
    ## 7 SleepHrsNight   -0.236    0.0612     -3.86 1.16e- 4

``` r
    #note on interpretation: if you are male, then your BMI is .8 less than female.

#WEIGHTED REGRESSION
est.weighted <- lm(BMI ~ Gender + Race1 + SleepHrsNight, weights=WTINT2YR, data=df)
tidy(est.weighted)
```

    ## # A tibble: 7 x 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    32.6       0.498     65.5   0       
    ## 2 Gendermale     -0.0887    0.172     -0.515 6.06e- 1
    ## 3 Race1Hispanic  -2.37      0.461     -5.15  2.75e- 7
    ## 4 Race1Mexican   -1.47      0.390     -3.77  1.62e- 4
    ## 5 Race1White     -2.46      0.277     -8.89  7.86e-19
    ## 6 Race1Other     -4.82      0.411    -11.7   2.13e-31
    ## 7 SleepHrsNight  -0.230     0.0647    -3.55  3.83e- 4

``` r
    #Observable difference in gender estimates
    #Differentials do not changes greatly

#READ IN DATA FROM CSV
df.auto <- read_csv('https://tyleransom.github.io/teaching/MetricsLabs/auto.csv')

#LOG PRICE MUTATION (df.auto)
df.auto %<>% mutate(log.price = log(price), foreign = as.factor(foreign))
#REGRESS
est.auto <- lm(log.price ~ weight + foreign, data=df.auto)
tidy(est.auto)
```

    ## # A tibble: 3 x 5
    ##   term           estimate std.error statistic  p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    7.09     0.170         41.7  1.07e-51
    ## 2 weight         0.000461 0.0000500      9.21 9.44e-14
    ## 3 foreignForeign 0.535    0.0844         6.34 1.85e- 8

``` r
#HETERO ROBUST-INF REGRESS
est.rob.auto <- lm_robust(log.price ~ weight + foreign, data=df.auto)
tidy(est.rob.auto)
```

    ##             term     estimate    std.error statistic      p.value     conf.low
    ## 1    (Intercept) 7.0908574677 2.035467e-01 34.836521 2.215096e-46 6.6849969588
    ## 2         weight 0.0004605598 6.095555e-05  7.555666 1.112395e-10 0.0003390179
    ## 3 foreignForeign 0.5352669467 8.732992e-02  6.129250 4.435173e-08 0.3611360225
    ##      conf.high df   outcome
    ## 1 7.4967179767 71 log.price
    ## 2 0.0005821017 71 log.price
    ## 3 0.7093978710 71 log.price

``` r
est.clust.auto <- lm_robust(log.price ~ weight + foreign, data=df.auto,
                            clusters=df.auto$manufacturer)
linearHypothesis(est.clust.auto,c("weight=0","foreignForeign=0"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## weight = 0
    ## foreignForeign = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: log.price ~ weight + foreign
    ## 
    ##   Res.Df Df  Chisq Pr(>Chisq)    
    ## 1     73                         
    ## 2     71  2 29.333  4.269e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
glance(est.auto)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.548         0.535 0.267      43.0 5.70e-13     2  -5.83  19.7  28.9
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
