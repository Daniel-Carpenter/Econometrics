Econometrics Technical Project
================
Daniel Carpenter
Spring 2019

``` r
library(tidyverse)
library(broom)
library(wooldridge)
library(skimr)

##################C1 DATA###################
t1 <- as_tibble(bwght)

#CREATE FEMALE VARIABLE
t1 <- t1 %>% mutate(female=male<1)

#FEMALE DISTRIBUTIOn; n=665, 47.91%
#summary(t1$female)
665/(665+723)
```

    ## [1] 0.4791066

``` r
#FEMALES SMOKING DURING PREGNANCY; n=112
#t1 <- t1 %>% filter(female>0,cigs>0)
#summary(t1$female)

#AVG # CIGS SMOKED BY PREGNANT WOMEN avg=12.41
#t1 <- t1 %>% filter(female>0,cigs>0)
skim(t1$cigs)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | t1$cigs |
| Number of rows                                   | 1388    |
| Number of columns                                | 1       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| numeric                                          | 1       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| data          |         0 |             1 | 2.09 | 5.97 |   0 |   0 |   0 |   0 |   50 | ▇▁▁▁▁ |

``` r
#AVG FATHER's EDUCATION
skim(t1$fatheduc)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | t1$fatheduc |
| Number of rows                                   | 1388        |
| Number of columns                                | 1           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| numeric                                          | 1           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|-----:|----:|----:|----:|----:|-----:|:------|
| data          |       196 |          0.86 | 13.19 | 2.75 |   1 |  12 |  12 |  16 |   18 | ▁▁▂▇▅ |

``` r
#AVG FAMILY INCOME: $29,030
#SD FAMILY INCOME: $18,740
skim(t1$faminc)
```

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | t1$faminc |
| Number of rows                                   | 1388      |
| Number of columns                                | 1         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| numeric                                          | 1         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|-----:|-----:|-----:|-----:|:------|
| data          |         0 |             1 | 29.03 | 18.74 | 0.5 | 14.5 | 27.5 | 37.5 |   65 | ▆▇▇▃▅ |

``` r
################C3 DATA####################
t2 <- as_tibble(meap01)

#MAX=0%, MIN=100% (RANGE OF MATH04)
max(t2$math4)
```

    ## [1] 100

``` r
min(t2$math4)
```

    ## [1] 0

``` r
#NUMBER OF SCHOOLS WITH PERFECT MATH PASS RATES
#t2 <- t2 %>% filter(math4==100)
skim(t2$math4)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | t2$math4 |
| Number of rows                                   | 1823     |
| Number of columns                                | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 |  p25 |  p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|-----:|-----:|----:|-----:|:------|
| data          |         0 |             1 | 71.91 | 19.95 |   0 | 61.6 | 76.4 |  87 |  100 | ▁▁▃▆▇ |

``` r
#n=38
# % of total: 2.08%
38/1823
```

    ## [1] 0.02084476

``` r
#Schools with 50% pass rates
#t2 <- t2 %>% filter(math4==50)
skim(t2$math4)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | t2$math4 |
| Number of rows                                   | 1823     |
| Number of columns                                | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 |  p25 |  p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|-----:|-----:|----:|-----:|:------|
| data          |         0 |             1 | 71.91 | 19.95 |   0 | 61.6 | 76.4 |  87 |  100 | ▁▁▃▆▇ |

``` r
#n=17

#AVG MATH VS READING SCORE
skim(t2$math4) #avg=71.91
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | t2$math4 |
| Number of rows                                   | 1823     |
| Number of columns                                | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 |  p25 |  p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|-----:|-----:|----:|-----:|:------|
| data          |         0 |             1 | 71.91 | 19.95 |   0 | 61.6 | 76.4 |  87 |  100 | ▁▁▃▆▇ |

``` r
skim(t2$read4) #avg=60.06
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | t2$read4 |
| Number of rows                                   | 1823     |
| Number of columns                                | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|----:|-----:|-----:|-----:|-----:|:------|
| data          |         0 |             1 | 60.06 | 19.15 |   0 | 48.9 | 62.7 | 73.9 |  100 | ▁▂▆▇▂ |

``` r
#CORRELATION B/W MATH AND READING SCORES
cov(t2$read4,t2$math4)/var(t2$math4)
```

    ## [1] 0.8086543

``` r
#AVG, SD EXPENDITURE PER PUPIL
skim(t2$exppp) 
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | t2$exppp |
| Number of rows                                   | 1823     |
| Number of columns                                | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 1        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |      sd |      p0 |     p25 |     p50 |     p75 |     p100 | hist  |
|:--------------|----------:|--------------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|:------|
| data          |         0 |             1 | 5194.87 | 1091.89 | 1206.88 | 4501.54 | 5078.25 | 5767.14 | 11957.64 | ▁▇▃▁▁ |

``` r
#avg=$5,194.87
#SD=$1,091.89

#5500 vs 6000
6000/5500 #9.09% increase in exppp
```

    ## [1] 1.090909

``` r
log(6000)-log(5500)
```

    ## [1] 0.08701138

``` r
(6000/5500-1)-(log(6000)-log(5500))
```

    ## [1] 0.003897714
