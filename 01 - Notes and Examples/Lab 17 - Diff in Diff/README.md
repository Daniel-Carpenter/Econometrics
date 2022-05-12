Econometrics Lab
================
Daniel Carpenter
Spring 2019

``` r
#KEY: Interaction variables shows differences in differences
# Basic ideas: when you add in more variables, your interaction 
  #variable's coef will go down, which gives DiD


library(tidyverse)
library(wooldridge)
library(broom)
library(magrittr)
library(stargazer)
library(clubSandwich)
library(lmtest)

?injury
df <- as_tibble(injury)
df %<>% filter(ky==1)


# is there even a effect?

df %>% group_by(afchnge,highearn) %>% 
        summarize(mean.ldurat = mean(ldurat))
```

    ## # A tibble: 4 x 3
    ## # Groups:   afchnge [2]
    ##   afchnge highearn mean.ldurat
    ##     <int>    <int>       <dbl>
    ## 1       0        0        1.13
    ## 2       0        1        1.38
    ## 3       1        0        1.13
    ## 4       1        1        1.58

``` r
did <- (1.58 - 1.38) - (1.13 - 1.13)
did
```

    ## [1] 0.2

``` r
#INTERPRETATION: Policy change increased benefits
                #by 20%

#Make factor variables
df %<>% mutate(afchnge = as.factor(afchnge),
               highearn = as.factor(highearn))

#REgress
est.did <- lm(ldurat ~ afchnge*highearn,data=df)
tidy(est.did)
```

    ## # A tibble: 4 x 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         1.13       0.0307    36.6   1.62e-263
    ## 2 afchnge1            0.00766    0.0447     0.171 8.64e-  1
    ## 3 highearn1           0.256      0.0474     5.41  6.72e-  8
    ## 4 afchnge1:highearn1  0.191      0.0685     2.78  5.42e-  3

``` r
#REgress
#est.did.rob <- lm_robust(ldurat ~ afchnge*highearn,data=df)
#tidy(est.did)

#INterpretation
#policy caused 19% increase in weeks on benefits:
#statistically significant in weeks earned


#First format indust and injtype
df %<>% mutate(inust = as.factor(indust),
               injtype = as.factor(injtype))


est.did.x <- lm(ldurat ~ afchnge*highearn
                       + male + married + hosp
                       + I(age^2) + indust
                       + injtype + lprewage,data=df)
tidy(est.did.x)
```

    ## # A tibble: 17 x 5
    ##    term                 estimate std.error statistic   p.value
    ##    <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)        -1.62      0.424         -3.83 1.30e-  4
    ##  2 afchnge1            0.0510    0.0413         1.23 2.17e-  1
    ##  3 highearn1          -0.162     0.0891        -1.81 6.97e-  2
    ##  4 male               -0.0807    0.0418        -1.93 5.34e-  2
    ##  5 married             0.0647    0.0369         1.75 8.01e-  2
    ##  6 hosp                1.13      0.0370        30.7  2.47e-190
    ##  7 I(age^2)            0.0000741 0.0000166      4.47 8.13e-  6
    ##  8 indust              0.0748    0.0187         4.00 6.34e-  5
    ##  9 injtype2            0.932     0.144          6.48 1.02e- 10
    ## 10 injtype3            0.636     0.0855         7.44 1.18e- 13
    ## 11 injtype4            0.557     0.0929         6.00 2.13e-  9
    ## 12 injtype5            0.642     0.0855         7.51 6.78e- 14
    ## 13 injtype6            0.617     0.0864         7.14 1.05e- 12
    ## 14 injtype7            0.990     0.191          5.19 2.16e-  7
    ## 15 injtype8            0.437     0.119          3.67 2.46e-  4
    ## 16 lprewage            0.314     0.0796         3.94 8.23e-  5
    ## 17 afchnge1:highearn1  0.163     0.0639         2.55 1.09e-  2

``` r
stargazer(est.did,est.did.x,type="text")
```

    ## 
    ## ======================================================================
    ##                                    Dependent variable:                
    ##                     --------------------------------------------------
    ##                                           ldurat                      
    ##                               (1)                       (2)           
    ## ----------------------------------------------------------------------
    ## afchnge1                     0.008                     0.051          
    ##                             (0.045)                   (0.041)         
    ##                                                                       
    ## highearn1                   0.256***                  -0.162*         
    ##                             (0.047)                   (0.089)         
    ##                                                                       
    ## male                                                  -0.081*         
    ##                                                       (0.042)         
    ##                                                                       
    ## married                                               0.065*          
    ##                                                       (0.037)         
    ##                                                                       
    ## hosp                                                 1.134***         
    ##                                                       (0.037)         
    ##                                                                       
    ## I(age2)                                              0.0001***        
    ##                                                      (0.00002)        
    ##                                                                       
    ## indust                                               0.075***         
    ##                                                       (0.019)         
    ##                                                                       
    ## injtype2                                             0.932***         
    ##                                                       (0.144)         
    ##                                                                       
    ## injtype3                                             0.636***         
    ##                                                       (0.086)         
    ##                                                                       
    ## injtype4                                             0.557***         
    ##                                                       (0.093)         
    ##                                                                       
    ## injtype5                                             0.642***         
    ##                                                       (0.085)         
    ##                                                                       
    ## injtype6                                             0.617***         
    ##                                                       (0.086)         
    ##                                                                       
    ## injtype7                                             0.990***         
    ##                                                       (0.191)         
    ##                                                                       
    ## injtype8                                             0.437***         
    ##                                                       (0.119)         
    ##                                                                       
    ## lprewage                                             0.314***         
    ##                                                       (0.080)         
    ##                                                                       
    ## afchnge1:highearn1          0.191***                  0.163**         
    ##                             (0.069)                   (0.064)         
    ##                                                                       
    ## Constant                    1.126***                 -1.623***        
    ##                             (0.031)                   (0.424)         
    ##                                                                       
    ## ----------------------------------------------------------------------
    ## Observations                 5,626                     5,347          
    ## R2                           0.021                     0.189          
    ## Adjusted R2                  0.020                     0.186          
    ## Residual Std. Error    1.269 (df = 5622)         1.150 (df = 5330)    
    ## F Statistic         39.540*** (df = 3; 5622) 77.411*** (df = 16; 5330)
    ## ======================================================================
    ## Note:                                      *p<0.1; **p<0.05; ***p<0.01
