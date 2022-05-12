Econometrics Technical Project
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(readxl)
library(skimr)
library(broom)
library(car)
library(pdfetch)
library(zoo)
library(lmtest)
library(sandwich)
library(dynlm)
library(estimatr)
library(car)
library(lmtest)
library(magrittr)
library(NHANES)
library(tsibble)
library(wooldridge)
library(tseries)



##### 11.C10 #####

df <- as_tibble(phillips)
#?phillips
  
#Estimate normal phillips and lag
est <- lm(inf  ~ unem,data=df)
est.delta <- lm(cinf  ~ unem,data=df)

tidy(est)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    1.05      1.55      0.681  0.499 
    ## 2 unem           0.502     0.266     1.89   0.0639

``` r
tidy(est.delta)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    2.83      1.22       2.31  0.0249
    ## 2 unem          -0.518     0.209     -2.48  0.0165

``` r
#Natural rate unem
1.05/0.502 
```

    ## [1] 2.091633

``` r
2.83/-0.518 
```

    ## [1] -5.46332

``` r
#Test for Unit root
adf.test(df$unem)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  df$unem
    ## Dickey-Fuller = -2.2056, Lag order = 3, p-value = 0.4917
    ## alternative hypothesis: stationary

``` r
#Obtain R2 of cunem as explanatory variable
est.delta.2 <- lm(cinf  ~ cunem,data=df)
summary(est.delta)
```

    ## 
    ## Call:
    ## lm(formula = cinf ~ unem, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0741 -0.9241  0.0189  0.8606  5.4800 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   2.8282     1.2249   2.309   0.0249 *
    ## unem         -0.5176     0.2090  -2.476   0.0165 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.307 on 53 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1037, Adjusted R-squared:  0.08679 
    ## F-statistic: 6.132 on 1 and 53 DF,  p-value: 0.0165

``` r
summary(est.delta.2)
```

    ## 
    ## Call:
    ## lm(formula = cinf ~ cunem, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4790 -0.9441  0.1384  1.0889  5.4551 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -0.07214    0.30584  -0.236  0.81443   
    ## cunem       -0.83281    0.28984  -2.873  0.00583 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.267 on 53 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1348, Adjusted R-squared:  0.1185 
    ## F-statistic: 8.256 on 1 and 53 DF,  p-value: 0.005831

``` r
##### 18.C3 #####

df1 <- as_tibble(volat)
?volat

#Estimate AR(3) model on "pcip"
est.pcip <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3,data=df1)
summary(est.pcip)
```

    ## 
    ## Call:
    ## lm(formula = pcip ~ pcip_1 + pcip_2 + pcip_3, data = df1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -49.208  -6.506  -0.187   5.625  84.817 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.80419    0.54804   3.292  0.00106 ** 
    ## pcip_1       0.34912    0.04252   8.210 1.58e-15 ***
    ## pcip_2       0.07080    0.04495   1.575  0.11582    
    ## pcip_3       0.06737    0.04253   1.584  0.11373    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.15 on 550 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.1657, Adjusted R-squared:  0.1611 
    ## F-statistic:  36.4 on 3 and 550 DF,  p-value: < 2.2e-16

``` r
#Add in 4th lag to above model
est.pcip.2 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + lag(pcip,4),data=df1)
summary(est.pcip.2)
```

    ## 
    ## Call:
    ## lm(formula = pcip ~ pcip_1 + pcip_2 + pcip_3 + lag(pcip, 4), 
    ##     data = df1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -49.164  -6.525  -0.181   5.638  84.847 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.787332   0.554904   3.221  0.00135 ** 
    ## pcip_1       0.349382   0.042716   8.179    2e-15 ***
    ## pcip_2       0.070236   0.045132   1.556  0.12022    
    ## pcip_3       0.065750   0.045128   1.457  0.14570    
    ## lag(pcip, 4) 0.004317   0.042696   0.101  0.91950    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.17 on 548 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.1659, Adjusted R-squared:  0.1598 
    ## F-statistic: 27.24 on 4 and 548 DF,  p-value: < 2.2e-16

``` r
#Estimate AR(3) model on "pcip" (w/pscp)
est.pcip.3 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + pcsp_1 + pcsp_2 + pcsp_3,data=df1)
summary(est.pcip.3)
```

    ## 
    ## Call:
    ## lm(formula = pcip ~ pcip_1 + pcip_2 + pcip_3 + pcsp_1 + pcsp_2 + 
    ##     pcsp_3, data = df1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -51.470  -6.586  -0.222   5.652  83.251 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.24554    0.56066   2.222   0.0267 *  
    ## pcip_1       0.32584    0.04254   7.660 8.52e-14 ***
    ## pcip_2       0.06910    0.04448   1.553   0.1209    
    ## pcip_3       0.07995    0.04215   1.897   0.0584 .  
    ## pcsp_1       0.02345    0.01313   1.786   0.0747 .  
    ## pcsp_2       0.03233    0.01351   2.393   0.0170 *  
    ## pcsp_3       0.01959    0.01321   1.484   0.1385    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.01 on 547 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.1895, Adjusted R-squared:  0.1806 
    ## F-statistic: 21.32 on 6 and 547 DF,  p-value: < 2.2e-16

``` r
#Estimate AR(3) model on "pcip" (w/pscp and i3)
est.pcip.4 <- lm(pcip ~ pcip_1 + pcip_2 + pcip_3 + 
                   pcsp_1 + pcsp_2 + pcsp_3 +
                   ci3 + ci3_1 + ci3_2,data=df1)
summary(est.pcip.4)
```

    ## 
    ## Call:
    ## lm(formula = pcip ~ pcip_1 + pcip_2 + pcip_3 + pcsp_1 + pcsp_2 + 
    ##     pcsp_3 + ci3 + ci3_1 + ci3_2, data = df1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -50.851  -6.205  -0.102   5.630  82.162 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.42708    0.55741   2.560  0.01073 *  
    ## pcip_1       0.30172    0.04278   7.052 5.37e-12 ***
    ## pcip_2       0.04875    0.04456   1.094  0.27449    
    ## pcip_3       0.07579    0.04183   1.812  0.07054 .  
    ## pcsp_1       0.02639    0.01332   1.981  0.04809 *  
    ## pcsp_2       0.02647    0.01371   1.931  0.05402 .  
    ## pcsp_3       0.01765    0.01327   1.329  0.18427    
    ## ci3          3.57487    1.11803   3.197  0.00147 ** 
    ## ci3_1        0.58382    1.16459   0.501  0.61635    
    ## ci3_2        1.72886    1.13868   1.518  0.12952    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.89 on 544 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.2101, Adjusted R-squared:  0.197 
    ## F-statistic: 16.08 on 9 and 544 DF,  p-value: < 2.2e-16
