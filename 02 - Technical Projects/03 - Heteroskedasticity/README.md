Econometrics Technical Project
================
Daniel Carpenter
Spring 2019

``` r
##############Computer Exercise 4.C12##################
library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(magrittr)
library(NHANES)
library(estimatr)
library(skimr)
library(wooldridge)

df <- as_tibble(econmath)

est <- lm(colgpa ~ hsgpa + actmth + acteng,data=df)
tidy(est)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   0.0282   0.168       0.168 8.66e- 1
    ## 2 hsgpa         0.659    0.0530     12.4   1.47e-32
    ## 3 actmth        0.0130   0.00515     2.53  1.17e- 2
    ## 4 acteng        0.0122   0.00504     2.42  1.56e- 2

``` r
#Orginal
.0282+.659*(1)+.0130*(1)+.0122*(1)
```

    ## [1] 0.7124

``` r
#1 unit change hrsgpa - A 0.343 point increase in high school GPA leads to a (1.3714-0.938437) = 0.432963 increase in GPA
.0282+.659*(1.343)+.0130*(1)+.0122*(1)
```

    ## [1] 0.938437

``` r
#Change ACT scores to see same effect
.0282+.659*(1)+.0130*(9.9)+.0122*(9.9)
```

    ## [1] 0.93668

``` r
(1.3714-0.938437)
```

    ## [1] 0.432963

``` r
#ACTMATH have same effect as ACTEND?
linearHypothesis(est, c("actmth=acteng"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## actmth - acteng = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: colgpa ~ hsgpa + actmth + acteng
    ## 
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    811 175.92                           
    ## 2    810 175.92  1 0.0019127 0.0088 0.9253

``` r
#LARGEST INFLUENCER ON COLGPA
est <- lm(colgpa ~ age + work + study + econhs  + hsgpa + acteng + actmth + act + mathscr + male + calculus + attexc + attgood + fathcoll + mothcoll + score,data=df)
tidy(est)
```

    ## # A tibble: 17 x 5
    ##    term           estimate std.error  statistic  p.value
    ##    <chr>             <dbl>     <dbl>      <dbl>    <dbl>
    ##  1 (Intercept)  0.00000899   0.338    0.0000266 1.00e+ 0
    ##  2 age         -0.0118       0.0154  -0.765     4.45e- 1
    ##  3 work        -0.00220      0.00162 -1.36      1.75e- 1
    ##  4 study        0.0000708    0.00189  0.0375    9.70e- 1
    ##  5 econhs      -0.0489       0.0294  -1.66      9.70e- 2
    ##  6 hsgpa        0.413        0.0493   8.39      2.23e-16
    ##  7 acteng       0.00585      0.00727  0.804     4.22e- 1
    ##  8 actmth      -0.00489      0.00624 -0.784     4.33e- 1
    ##  9 act          0.0101       0.0105   0.962     3.37e- 1
    ## 10 mathscr      0.00333      0.00949  0.350     7.26e- 1
    ## 11 male        -0.0785       0.0304  -2.58      1.01e- 2
    ## 12 calculus    -0.0289       0.0331  -0.872     3.83e- 1
    ## 13 attexc       0.291        0.0516   5.64      2.42e- 8
    ## 14 attgood      0.123        0.0462   2.67      7.83e- 3
    ## 15 fathcoll     0.00160      0.0301   0.0532    9.58e- 1
    ## 16 mothcoll     0.0752       0.0313   2.41      1.64e- 2
    ## 17 score        0.0174       0.00125 13.9       1.89e-39

``` r
#################Computer Exercise 8.C4##################

df2 <- as_tibble(vote1)
#?vote1

est2 <- lm(voteA  ~ prtystrA + democA + lexpendA + lexpendB,data=df2)
tidy(est2)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   37.7      4.74        7.95 2.56e-13
    ## 2 prtystrA       0.252    0.0713      3.53 5.30e- 4
    ## 3 democA         3.79     1.41        2.70 7.72e- 3
    ## 4 lexpendA       5.78     0.392      14.7  4.03e-32
    ## 5 lexpendB      -6.24     0.397     -15.7  9.34e-35

``` r
summary(est2)
```

    ## 
    ## Call:
    ## lm(formula = voteA ~ prtystrA + democA + lexpendA + lexpendB, 
    ##     data = df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.576  -4.864  -1.146   4.903  24.566 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 37.66142    4.73604   7.952 2.56e-13 ***
    ## prtystrA     0.25192    0.07129   3.534  0.00053 ***
    ## democA       3.79294    1.40652   2.697  0.00772 ** 
    ## lexpendA     5.77929    0.39182  14.750  < 2e-16 ***
    ## lexpendB    -6.23784    0.39746 -15.694  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.573 on 168 degrees of freedom
    ## Multiple R-squared:  0.8012, Adjusted R-squared:  0.7964 
    ## F-statistic: 169.2 on 4 and 168 DF,  p-value: < 2.2e-16

``` r
est2.error <- lm(voteA ~ prtystrA + democA + lexpendA + lexpendB,data=df2)
tidy(est2)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   37.7      4.74        7.95 2.56e-13
    ## 2 prtystrA       0.252    0.0713      3.53 5.30e- 4
    ## 3 democA         3.79     1.41        2.70 7.72e- 3
    ## 4 lexpendA       5.78     0.392      14.7  4.03e-32
    ## 5 lexpendB      -6.24     0.397     -15.7  9.34e-35

``` r
summary(est2.error)
```

    ## 
    ## Call:
    ## lm(formula = voteA ~ prtystrA + democA + lexpendA + lexpendB, 
    ##     data = df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.576  -4.864  -1.146   4.903  24.566 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 37.66142    4.73604   7.952 2.56e-13 ***
    ## prtystrA     0.25192    0.07129   3.534  0.00053 ***
    ## democA       3.79294    1.40652   2.697  0.00772 ** 
    ## lexpendA     5.77929    0.39182  14.750  < 2e-16 ***
    ## lexpendB    -6.23784    0.39746 -15.694  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.573 on 168 degrees of freedom
    ## Multiple R-squared:  0.8012, Adjusted R-squared:  0.7964 
    ## F-statistic: 169.2 on 4 and 168 DF,  p-value: < 2.2e-16

``` r
#BP TEST on est2
bptest(est2)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  est2
    ## BP = 9.0934, df = 4, p-value = 0.05881

``` r
#WHITE TEST on est2
bptest(est2, ~ fitted(est2) + I(fitted(est2)^2) )
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  est2
    ## BP = 5.49, df = 2, p-value = 0.06425

``` r
#################Computer Exercise 8.C7##################

df3 <- as_tibble(loanapp)
#?loanapp

#NON-ROBUST MODEL
est3 <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,data=df3)
tidy(est3)
```

    ## # A tibble: 16 x 5
    ##    term        estimate std.error statistic  p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)  0.937     0.0527     17.8   1.47e-65
    ##  2 white        0.129     0.0197      6.53  8.44e-11
    ##  3 hrat         0.00183   0.00126     1.45  1.47e- 1
    ##  4 obrat       -0.00543   0.00110    -4.93  8.92e- 7
    ##  5 loanprc     -0.147     0.0375     -3.93  8.92e- 5
    ##  6 unem        -0.00730   0.00320    -2.28  2.26e- 2
    ##  7 male        -0.00414   0.0189     -0.220 8.26e- 1
    ##  8 married      0.0458    0.0163      2.81  5.00e- 3
    ##  9 dep         -0.00683   0.00670    -1.02  3.08e- 1
    ## 10 sch          0.00175   0.0166      0.105 9.16e- 1
    ## 11 cosign       0.00977   0.0411      0.238 8.12e- 1
    ## 12 chist        0.133     0.0193      6.91  6.72e-12
    ## 13 pubrec      -0.242     0.0282     -8.57  2.06e-17
    ## 14 mortlat1    -0.0573    0.0500     -1.14  2.52e- 1
    ## 15 mortlat2    -0.114     0.0670     -1.70  8.97e- 2
    ## 16 vr          -0.0314    0.0140     -2.24  2.52e- 2

``` r
#ROBUST MODEL
est3.rob <- lm_robust(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,data=df3)
tidy(est3.rob)
```

    ##           term     estimate   std.error  statistic      p.value     conf.low
    ## 1  (Intercept)  0.936731196 0.059668344 15.6989642 2.121743e-52  0.819710943
    ## 2        white  0.128819621 0.025925020  4.9689305 7.318486e-07  0.077976039
    ## 3         hrat  0.001832991 0.001472688  1.2446569 2.134071e-01 -0.001055212
    ## 4        obrat -0.005431798 0.001338403 -4.0584172 5.135120e-05 -0.008056644
    ## 5      loanprc -0.147300063 0.038130262 -3.8630751 1.156159e-04 -0.222080300
    ## 6         unem -0.007298934 0.003727010 -1.9583888 5.032642e-02 -0.014608264
    ## 7         male -0.004144133 0.019332959 -0.2143559 8.302919e-01 -0.042059511
    ## 8      married  0.045824083 0.017267160  2.6538285 8.022910e-03  0.011960105
    ## 9          dep -0.006827370 0.006925761 -0.9857934 3.243565e-01 -0.020410021
    ## 10         sch  0.001752507 0.017175670  0.1020343 9.187399e-01 -0.031932042
    ## 11      cosign  0.009772214 0.039949435  0.2446146 8.067805e-01 -0.068575745
    ## 12       chist  0.133026744 0.024686980  5.3885387 7.961267e-08  0.084611179
    ## 13      pubrec -0.241926783 0.042981599 -5.6286129 2.079044e-08 -0.326221356
    ## 14    mortlat1 -0.057251114 0.067227032 -0.8516085 3.945357e-01 -0.189095300
    ## 15    mortlat2 -0.113723392 0.093287540 -1.2190630 2.229673e-01 -0.296676878
    ## 16          vr -0.031440761 0.014502593 -2.1679407 3.028350e-02 -0.059882929
    ##        conf.high   df outcome
    ## 1   1.053751e+00 1955 approve
    ## 2   1.796632e-01 1955 approve
    ## 3   4.721195e-03 1955 approve
    ## 4  -2.806951e-03 1955 approve
    ## 5  -7.251983e-02 1955 approve
    ## 6   1.039592e-05 1955 approve
    ## 7   3.377124e-02 1955 approve
    ## 8   7.968806e-02 1955 approve
    ## 9   6.755282e-03 1955 approve
    ## 10  3.543706e-02 1955 approve
    ## 11  8.812017e-02 1955 approve
    ## 12  1.814423e-01 1955 approve
    ## 13 -1.576322e-01 1955 approve
    ## 14  7.459307e-02 1955 approve
    ## 15  6.923009e-02 1955 approve
    ## 16 -2.998593e-03 1955 approve
