Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#1. STARGAZER SUMMARY STATS
#2. DIFFERENT WAY TO ADD EXP VARIABLE I(age^2)
#3. #TEST RELEVANCE OF IV

library(tidyverse)
library(wooldridge)
library(broom)
library(AER)
library(magrittr)
library(stargazer)
library(skimr)

?fertil2
df <- as_tibble(fertil2)

#SUMMARY STATS WITH SKIMR
skim(df)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | df   |
| Number of rows                                   | 4361 |
| Number of columns                                | 27   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| numeric                                          | 27   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |     sd |  p0 | p25 | p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|-------:|-------:|----:|----:|----:|-----:|-----:|:------|
| mnthborn      |         0 |          1.00 |   6.33 |   3.32 |   1 |   3 |   6 |    9 |   12 | ▇▅▇▆▆ |
| yearborn      |         0 |          1.00 |  60.43 |   8.68 |  38 |  55 |  62 |   68 |   73 | ▂▃▆▇▇ |
| age           |         0 |          1.00 |  27.41 |   8.69 |  15 |  20 |  26 |   33 |   49 | ▇▇▆▃▂ |
| electric      |         3 |          1.00 |   0.14 |   0.35 |   0 |   0 |   0 |    0 |    1 | ▇▁▁▁▁ |
| radio         |         2 |          1.00 |   0.70 |   0.46 |   0 |   0 |   1 |    1 |    1 | ▃▁▁▁▇ |
| tv            |         2 |          1.00 |   0.09 |   0.29 |   0 |   0 |   0 |    0 |    1 | ▇▁▁▁▁ |
| bicycle       |         3 |          1.00 |   0.28 |   0.45 |   0 |   0 |   0 |    1 |    1 | ▇▁▁▁▃ |
| educ          |         0 |          1.00 |   5.86 |   3.93 |   0 |   3 |   7 |    8 |   20 | ▆▇▅▁▁ |
| ceb           |         0 |          1.00 |   2.44 |   2.41 |   0 |   1 |   2 |    4 |   13 | ▇▃▁▁▁ |
| agefbrth      |      1088 |          0.75 |  19.01 |   3.09 |  10 |  17 |  19 |   20 |   38 | ▁▇▂▁▁ |
| children      |         0 |          1.00 |   2.27 |   2.22 |   0 |   0 |   2 |    4 |   13 | ▇▃▁▁▁ |
| knowmeth      |         7 |          1.00 |   0.96 |   0.19 |   0 |   1 |   1 |    1 |    1 | ▁▁▁▁▇ |
| usemeth       |        71 |          0.98 |   0.58 |   0.49 |   0 |   0 |   1 |    1 |    1 | ▆▁▁▁▇ |
| monthfm       |      2282 |          0.48 |   6.27 |   3.62 |   1 |   3 |   6 |    9 |   12 | ▇▃▅▅▇ |
| yearfm        |      2282 |          0.48 |  76.91 |   7.76 |  50 |  72 |  78 |   83 |   88 | ▁▂▃▇▇ |
| agefm         |      2282 |          0.48 |  20.69 |   5.00 |  10 |  17 |  20 |   23 |   46 | ▅▇▂▁▁ |
| idlnchld      |       120 |          0.97 |   4.62 |   2.22 |   0 |   3 |   4 |    6 |   20 | ▇▅▁▁▁ |
| heduc         |      2405 |          0.45 |   5.14 |   4.80 |   0 |   0 |   6 |    8 |   20 | ▇▅▃▁▁ |
| agesq         |         0 |          1.00 | 826.46 | 526.92 | 225 | 400 | 676 | 1089 | 2401 | ▇▅▂▁▁ |
| urban         |         0 |          1.00 |   0.52 |   0.50 |   0 |   0 |   1 |    1 |    1 | ▇▁▁▁▇ |
| urb_educ      |         0 |          1.00 |   3.47 |   4.29 |   0 |   0 |   0 |    7 |   20 | ▇▃▂▁▁ |
| spirit        |         0 |          1.00 |   0.42 |   0.49 |   0 |   0 |   0 |    1 |    1 | ▇▁▁▁▆ |
| protest       |         0 |          1.00 |   0.23 |   0.42 |   0 |   0 |   0 |    0 |    1 | ▇▁▁▁▂ |
| catholic      |         0 |          1.00 |   0.10 |   0.30 |   0 |   0 |   0 |    0 |    1 | ▇▁▁▁▁ |
| frsthalf      |         0 |          1.00 |   0.54 |   0.50 |   0 |   0 |   1 |    1 |    1 | ▇▁▁▁▇ |
| educ0         |         0 |          1.00 |   0.21 |   0.41 |   0 |   0 |   0 |    0 |    1 | ▇▁▁▁▂ |
| evermarr      |         0 |          1.00 |   0.48 |   0.50 |   0 |   0 |   0 |    1 |    1 | ▇▁▁▁▇ |

``` r
#SUMMARY STATS WITH STARGAZER
#df %>% as.data.frame %>% stargazer(type="text")


#Question: What do you think is going on when you see varying numbers of observations across the different variables?
    #Probably because premarital birth(1/2)

est.ols <- lm(children ~ educ + age + I(age^2), data=df)
tidy(est.ols)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept) -4.14     0.241       -17.2  3.33e-64
    ## 2 educ        -0.0906   0.00592     -15.3  1.68e-51
    ## 3 age          0.332    0.0165       20.1  6.52e-86
    ## 4 I(age^2)    -0.00263  0.000273     -9.65 8.03e-22

``` r
  #increasing your age by 1 year increseS your children by .332, but at a decreasing rate

#OTHER WAY TO DO TIDY(EST)
stargazer(est.ols, type="text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                              children          
    ## -----------------------------------------------
    ## educ                         -0.091***         
    ##                               (0.006)          
    ##                                                
    ## age                          0.332***          
    ##                               (0.017)          
    ##                                                
    ## I(age2)                      -0.003***         
    ##                              (0.0003)          
    ##                                                
    ## Constant                     -4.138***         
    ##                               (0.241)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   4,361           
    ## R2                             0.569           
    ## Adjusted R2                    0.568           
    ## Residual Std. Error      1.460 (df = 4357)     
    ## F Statistic         1,915.196*** (df = 3; 4357)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Create dummy if you born before July 1
df %<>% mutate(firsthalf = mnthborn<7)


#TEST RELEVANCE OF IV
est.iv <- ivreg(children ~ educ + age + I(age^2) | firsthalf + age + I(age^2), data=df)
est.iv1 <- ivreg(educ ~ firsthalf,data=df)
stargazer(est.ols,est.iv1,est.iv, type="text")
```

    ## 
    ## ===================================================================================
    ##                                           Dependent variable:                      
    ##                     ---------------------------------------------------------------
    ##                              children                 educ            children     
    ##                                 OLS               instrumental      instrumental   
    ##                                                     variable          variable     
    ##                                 (1)                    (2)               (3)       
    ## -----------------------------------------------------------------------------------
    ## educ                         -0.091***                                -0.171***    
    ##                               (0.006)                                  (0.053)     
    ##                                                                                    
    ## age                          0.332***                                 0.324***     
    ##                               (0.017)                                  (0.018)     
    ##                                                                                    
    ## I(age2)                      -0.003***                                -0.003***    
    ##                              (0.0003)                                 (0.0003)     
    ##                                                                                    
    ## firsthalf                                           -0.938***                      
    ##                                                      (0.118)                       
    ##                                                                                    
    ## Constant                     -4.138***              6.363***          -3.388***    
    ##                               (0.241)                (0.087)           (0.548)     
    ##                                                                                    
    ## -----------------------------------------------------------------------------------
    ## Observations                   4,361                  4,361             4,361      
    ## R2                             0.569                  0.014             0.550      
    ## Adjusted R2                    0.568                  0.014             0.550      
    ## Residual Std. Error      1.460 (df = 4357)      3.900 (df = 4359) 1.491 (df = 4357)
    ## F Statistic         1,915.196*** (df = 3; 4357)                                    
    ## ===================================================================================
    ## Note:                                                   *p<0.1; **p<0.05; ***p<0.01

## Comment on results

-   IV estimates seem to make sense
-   feducation causes education
-   IV SE is much larger than OLS SE
-   Is firstgald actually a good instrument
