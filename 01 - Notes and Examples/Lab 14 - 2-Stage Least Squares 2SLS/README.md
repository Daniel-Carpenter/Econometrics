Econometrics Lab
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
library(estimatr)

df <- as_tibble(mroz)

df %>% as.data.frame %>% stargazer(type="text")
```

    ## 
    ## =================================================
    ## Statistic  N     Mean     St. Dev.   Min    Max  
    ## -------------------------------------------------
    ## inlf      753   0.568      0.496      0      1   
    ## hours     753  740.576    871.314     0    4,950 
    ## kidslt6   753   0.238      0.524      0      3   
    ## kidsge6   753   1.353      1.320      0      8   
    ## age       753   42.538     8.073      30     60  
    ## educ      753   12.287     2.280      5      17  
    ## wage      428   4.178      3.310    0.128  25.000
    ## repwage   753   1.850      2.420    0.000  9.980 
    ## hushrs    753 2,267.271   595.567    175   5,010 
    ## husage    753   45.121     8.059      30     60  
    ## huseduc   753   12.491     3.021      3      17  
    ## huswage   753   7.482      4.231    0.412  40.509
    ## faminc    753 23,080.600 12,190.200 1,500  96,000
    ## mtr       753   0.679      0.083    0.442  0.942 
    ## motheduc  753   9.251      3.367      0      17  
    ## fatheduc  753   8.809      3.572      0      17  
    ## unem      753   8.624      3.115    3.000  14.000
    ## city      753   0.643      0.480      0      1   
    ## exper     753   10.631     8.069      0      45  
    ## nwifeinc  753   20.129     11.635   -0.029 96.000
    ## lwage     428   1.190      0.723    -2.054 3.219 
    ## expersq   753  178.039    249.631     0    2,025 
    ## -------------------------------------------------

``` r
?mroz

#Half observations for wages compared to total. Not a problem because we want to look at parameters for population of workers.

#Drop missing wages
df %<>% drop_na(wage)

est.stage1 <- lm(educ ~ motheduc + fatheduc + exper + I(exper^2), data=df)

#FIRST STAGE - F stat is large,so it is strong
linearHypothesis(est.stage1,c("motheduc","fatheduc"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## motheduc = 0
    ## fatheduc = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: educ ~ motheduc + fatheduc + exper + I(exper^2)
    ## 
    ##   Res.Df    RSS Df Sum of Sq    F    Pr(>F)    
    ## 1    425 2219.2                                
    ## 2    423 1758.6  2    460.64 55.4 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#SECOND STAGE
est.stage2 <- lm(log(wage) ~ fitted(est.stage1) + exper + I(exper^2), data=df)
# linearHypothesis(est.stage2,c("motheduc","fatheduc"))

#IV REG
est.2sls <- ivreg(log(wage) ~ educ + exper + I(exper^2) 
                            | motheduc + fatheduc + exper + I(exper^2), data=df)

#NOW OLS
est.ols <- lm(log(wage) ~ educ + exper + I(exper^2),data=df)

#FINAL ANSWER

stargazer(est.ols,est.stage2,est.2sls, type="text")
```

    ## 
    ## ==============================================================
    ##                                      Dependent variable:      
    ##                                -------------------------------
    ##                                           log(wage)           
    ##                                       OLS         instrumental
    ##                                                     variable  
    ##                                   (1)      (2)        (3)     
    ## --------------------------------------------------------------
    ## educ                           0.107***              0.061*   
    ##                                 (0.014)             (0.031)   
    ##                                                               
    ## fitted(est.stage1)                        0.061*              
    ##                                          (0.033)              
    ##                                                               
    ## exper                          0.042***  0.044***   0.044***  
    ##                                 (0.013)  (0.014)    (0.013)   
    ##                                                               
    ## I(exper2)                      -0.001**  -0.001**   -0.001**  
    ##                                (0.0004)  (0.0004)   (0.0004)  
    ##                                                               
    ## Constant                       -0.522***  0.048      0.048    
    ##                                 (0.199)  (0.420)    (0.400)   
    ##                                                               
    ## --------------------------------------------------------------
    ## Observations                      428      428        428     
    ## R2                               0.157    0.050      0.136    
    ## Adjusted R2                      0.151    0.043      0.130    
    ## Residual Std. Error (df = 424)   0.666    0.707      0.675    
    ## F Statistic (df = 3; 424)      26.286*** 7.405***             
    ## ==============================================================
    ## Note:                              *p<0.1; **p<0.05; ***p<0.01

``` r
#For the most part, the IV estimates do look good. However, parental educ may not meet exogenaity condition.
```
