Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
##### POOLED - RANDOM = FIXED EFFECTS #####

#POOLED, RANDOM, FIXED EFFECTS
#CODE TO SHOW COMPARISON

library(tidyverse)
library(wooldridge)
library(broom)
library(magrittr)
library(stargazer)
library(clubSandwich)
library(plm)

df <- as_tibble(wagepan)
df %<>% mutate(year=as.factor(year))
df %<>% rename(id = nr)

#?wagepan
pdim(df)
```

    ## Balanced Panel: n = 545, T = 8, N = 4360

``` r
df.within <- df %>% select(id,year,educ,married,union,rur) %>%
  group_by(id) %>%
  summarize(
    mean.edu = mean(educ),
     var.edu = var(educ), #shows if education changes for x person
    mean.marr = mean(married), #shows percentage of data that person is married
    var.marr = var(married),
    mean.union = mean(union),
    var.union = var(union),
    mean.rural = mean(rur),
    var.rural = var(rur)
  )
df.within %>% as.data.frame %>% stargazer(type="text")
```

    ## 
    ## ===============================================
    ## Statistic   N    Mean    St. Dev.   Min   Max  
    ## -----------------------------------------------
    ## id         545 5,262.059 3,498.960  13   12,548
    ## mean.edu   545  11.767     1.748     3     16  
    ## var.edu    545   0.000     0.000     0     0   
    ## mean.marr  545   0.439     0.377   0.000 1.000 
    ## var.marr   545   0.120     0.115   0.000 0.286 
    ## mean.union 545   0.244     0.329   0.000 1.000 
    ## var.union  545   0.087     0.105   0.000 0.286 
    ## mean.rural 545   0.204     0.359   0.000 1.000 
    ## var.rural  545   0.039     0.087   0.000 0.286 
    ## -----------------------------------------------

``` r
#1. no variation in personal education among years, married, union, rural = yes
#2. If positive value for mean variables, then they have once been involved with that activity within the years in this dataset
#3. It is important to understand if the dataset includes anything beyond 0 values. If so, then there is not much point in creating an estimate off the individuals.

#POOLED EFFECT
est.pols <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + rur + year,
                data = df, index = c("id","year"), model = "pooling")
tidy(est.pols)
```

    ## # A tibble: 16 x 5
    ##    term        estimate std.error statistic  p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  0.165    0.0784       2.10  3.60e- 2
    ##  2 educ         0.0873   0.00523     16.7   1.40e-60
    ##  3 black       -0.149    0.0235      -6.34  2.61e-10
    ##  4 hisp        -0.0158   0.0211      -0.750 4.54e- 1
    ##  5 exper        0.0693   0.0136       5.09  3.72e- 7
    ##  6 I(exper^2)  -0.00239  0.000815    -2.93  3.41e- 3
    ##  7 married      0.126    0.0158       7.98  1.79e-15
    ##  8 union        0.182    0.0171      10.7   3.00e-26
    ##  9 rur         -0.138    0.0188      -7.33  2.65e-13
    ## 10 year1981     0.0535   0.0302       1.77  7.64e- 2
    ## 11 year1982     0.0552   0.0330       1.67  9.49e- 2
    ## 12 year1983     0.0489   0.0365       1.34  1.80e- 1
    ## 13 year1984     0.0741   0.0399       1.86  6.36e- 2
    ## 14 year1985     0.0904   0.0432       2.09  3.62e- 2
    ## 15 year1986     0.120    0.0462       2.60  9.47e- 3
    ## 16 year1987     0.151    0.0492       3.07  2.18e- 3

``` r
#Interpretation of union: IF you are in a union, 
#then you have 18% greater wage than not being in a uion
    
#RANDOM EFFECTS
est.re <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + rur + year,
              data = df, index = c("id","year"), model = "random")
tidy(est.re)
```

    ## # A tibble: 16 x 5
    ##    term        estimate std.error statistic  p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  0.0362   0.149        0.243 8.08e- 1
    ##  2 educ         0.0913   0.0105       8.68  4.07e-18
    ##  3 black       -0.141    0.0471      -3.01  2.65e- 3
    ##  4 hisp         0.0162   0.0423       0.383 7.02e- 1
    ##  5 exper        0.106    0.0153       6.92  4.62e-12
    ##  6 I(exper^2)  -0.00468  0.000691    -6.77  1.33e-11
    ##  7 married      0.0653   0.0168       3.89  9.98e- 5
    ##  8 union        0.107    0.0178       6.00  1.96e- 9
    ##  9 rur         -0.0233   0.0241      -0.965 3.35e- 1
    ## 10 year1981     0.0401   0.0247       1.63  1.04e- 1
    ## 11 year1982     0.0304   0.0322       0.944 3.45e- 1
    ## 12 year1983     0.0191   0.0412       0.462 6.44e- 1
    ## 13 year1984     0.0414   0.0508       0.816 4.15e- 1
    ## 14 year1985     0.0558   0.0606       0.921 3.57e- 1
    ## 15 year1986     0.0893   0.0704       1.27  2.05e- 1
    ## 16 year1987     0.132    0.0803       1.64  1.01e- 1

``` r
#what are the differences in pooling model and random effect?
    #a. 

#5. The SE's in RE are smaller because there is serial correlation. RE has cluter robust SE (model = random (fix))

#FIXED EFFECTS

est.fe <- plm(lwage ~ I(exper^2) + married + union + rur + year,
              data = df, index = c("id","year"), model = "within")
tidy(est.fe)
```

    ## # A tibble: 11 x 5
    ##    term       estimate std.error statistic  p.value
    ##    <chr>         <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 I(exper^2) -0.00526  0.000706     -7.46 1.10e-13
    ##  2 married     0.0467   0.0183        2.55 1.08e- 2
    ##  3 union       0.0795   0.0193        4.12 3.95e- 5
    ##  4 rur         0.0493   0.0290        1.70 8.88e- 2
    ##  5 year1981    0.152    0.0219        6.93 5.10e-12
    ##  6 year1982    0.254    0.0244       10.4  4.93e-25
    ##  7 year1983    0.357    0.0293       12.2  1.37e-33
    ##  8 year1984    0.494    0.0363       13.6  3.24e-41
    ##  9 year1985    0.622    0.0453       13.7  6.89e-42
    ## 10 year1986    0.771    0.0562       13.7  7.59e-42
    ## 11 year1987    0.931    0.0689       13.5  9.41e-41

``` r
#Takes out random effects, focuses on only the fixed effects
    
    
#COMBINE THE RESULTS FOR COMPARISON: CLUSTER SE's
clust.po <- coef_test(est.pols, vcov = "CR1", cluster = "individual")
clust.re <- coef_test(est.re, vcov = "CR1", cluster = "individual")
clust.fe <- coef_test(est.fe, vcov = "CR1", cluster = "individual")

stargazer(est.pols,est.re,est.fe,se=list(clust.po$SE,clust.re$SE,clust.fe$SE),type="text")
```

    ## 
    ## ===========================================================================
    ##                                   Dependent variable:                      
    ##              --------------------------------------------------------------
    ##                                          lwage                             
    ##                         (1)               (2)                (3)           
    ## ---------------------------------------------------------------------------
    ## educ                 0.087***           0.091***                           
    ##                       (0.011)           (0.011)                            
    ##                                                                            
    ## black                -0.149***         -0.141***                           
    ##                       (0.050)           (0.051)                            
    ##                                                                            
    ## hisp                  -0.016             0.016                             
    ##                       (0.040)           (0.040)                            
    ##                                                                            
    ## exper                0.069***           0.106***                           
    ##                       (0.019)           (0.016)                            
    ##                                                                            
    ## I(exper2)            -0.002**          -0.005***          -0.005***        
    ##                       (0.001)           (0.001)            (0.001)         
    ##                                                                            
    ## married              0.126***           0.065***           0.047**         
    ##                       (0.026)           (0.019)            (0.021)         
    ##                                                                            
    ## union                0.182***           0.107***          0.079***         
    ##                       (0.027)           (0.021)            (0.023)         
    ##                                                                            
    ## rur                  -0.138***           -0.023             0.049          
    ##                       (0.035)           (0.031)            (0.039)         
    ##                                                                            
    ## year1981              0.053*             0.040            0.152***         
    ##                       (0.028)           (0.028)            (0.026)         
    ##                                                                            
    ## year1982               0.055             0.030            0.254***         
    ##                       (0.037)           (0.035)            (0.029)         
    ##                                                                            
    ## year1983               0.049             0.019            0.357***         
    ##                       (0.046)           (0.044)            (0.035)         
    ##                                                                            
    ## year1984               0.074             0.041            0.494***         
    ##                       (0.057)           (0.055)            (0.046)         
    ##                                                                            
    ## year1985               0.090             0.056            0.622***         
    ##                       (0.066)           (0.064)            (0.057)         
    ##                                                                            
    ## year1986               0.120             0.089            0.771***         
    ##                       (0.075)           (0.075)            (0.072)         
    ##                                                                            
    ## year1987              0.151*             0.132            0.931***         
    ##                       (0.084)           (0.085)            (0.085)         
    ##                                                                            
    ## Constant               0.165             0.036                             
    ##                       (0.161)           (0.161)                            
    ##                                                                            
    ## ---------------------------------------------------------------------------
    ## Observations           4,360             4,360              4,360          
    ## R2                     0.199             0.181              0.181          
    ## Adjusted R2            0.196             0.178              0.062          
    ## F Statistic  72.036*** (df = 15; 4344) 958.833*** 76.530*** (df = 11; 3804)
    ## ===========================================================================
    ## Note:                                           *p<0.1; **p<0.05; ***p<0.01

``` r
#INTERPRET UNION:
    #OLS: among all, being in a union leads to an 18% increase in wage
    #RE: "" all else constant, there is an 11% increase in wage from things like ability and what have you
    #FE: "" Unions by themselves give a 7% increase in wage 
```
