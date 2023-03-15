Lab 10 - Grading the professor, Pt. 2
================
Zheqi
3/15

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
evals <- openintro::evals
```

### Exercise 1

``` r
mbty <- lm(bty_avg ~ score, data = evals)
summary(mbty)
```

    ## 
    ## Call:
    ## lm(formula = bty_avg ~ score, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7116 -1.2116 -0.2032  0.9328  4.2089 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.2237     0.5409   4.111 4.66e-05 ***
    ## score         0.5256     0.1285   4.090 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.502 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
#mbty=2.2237+.5256bty_avg, R-squared:  0.03502, Adjusted R-squared:  0.03293 
```

### Exercise 2

``` r
m_bty_gen <-lm(score~bty_avg+gender, data=evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
#score=3.74734+.07bty_avg+.17(male),Multiple R-squared:  0.05912,   Adjusted R-squared:  0.05503 
```

``` r
#when bty_avg is 0 and gender is female, the score is 3.74734, does not have a meaning in this case.The slope means each increase in bty_avg, score increases .07
```

``` r
#5.9% is explained by the m_bty_gen.
```

``` r
#scores=3.91734+.07bty_avg
```

``` r
#males.
```

``` r
#higher beauty associated with higher scores, but males receive higher scores with the same level of beauty.
```

``` r
#r-adjusted increases because gender help predict scores.Gender explains extra 2.21% of variance in scores.
```

``` r
#adding gender flattens the slope of bty_avg.
```

``` r
m_bty_rank<-lm(score~bty_avg+rank, data=evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

``` r
#score=3.98155+.06783bty_avg-.1607_tenure_track/-.12632tenured
#the intercept is when bty_avg=0, teaching professors receive 3.98155.
#the slope is .06783, each increase in bty_avg increases .06783 in scores.
```

``` r
# think cls_student is not relevant to the score.
```

``` r
m_cls_st <-lm(score~cls_students, data=evals)
summary(m_cls_st)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_students, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8666 -0.3677  0.1281  0.4300  0.8336 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.1643491  0.0314034 132.608   <2e-16 ***
    ## cls_students 0.0001881  0.0003373   0.558    0.577    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5443 on 461 degrees of freedom
    ## Multiple R-squared:  0.0006744,  Adjusted R-squared:  -0.001493 
    ## F-statistic: 0.3111 on 1 and 461 DF,  p-value: 0.5773

``` r
#score=4.1643491+.0001881cls_students
#Multiple R-squared:  0.0006744,    Adjusted R-squared:  -0.001493 
```

``` r
#age. I think it does not predict scores.
```

``` r
full_model<-lm(score~rank+ ethnicity+ gender+ language+ cls_perc_eval+ cls_did_eval+ cls_students+ cls_level+ cls_profs+ cls_credits+ bty_avg, data=evals)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + cls_perc_eval + 
    ##     cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.86123 -0.30847  0.09617  0.35855  1.07038 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.245182   0.200828  16.159  < 2e-16 ***
    ## ranktenure track      -0.025541   0.074312  -0.344  0.73124    
    ## ranktenured           -0.048022   0.065492  -0.733  0.46379    
    ## ethnicitynot minority  0.182821   0.078001   2.344  0.01952 *  
    ## gendermale             0.164124   0.051263   3.202  0.00146 ** 
    ## languagenon-english   -0.158286   0.107760  -1.469  0.14257    
    ## cls_perc_eval          0.004798   0.002114   2.270  0.02370 *  
    ## cls_did_eval           0.002481   0.003122   0.795  0.42726    
    ## cls_students          -0.001063   0.001980  -0.537  0.59159    
    ## cls_levelupper         0.001159   0.056864   0.020  0.98374    
    ## cls_profssingle       -0.007187   0.051812  -0.139  0.88974    
    ## cls_creditsone credit  0.503797   0.117576   4.285 2.24e-05 ***
    ## bty_avg                0.068307   0.016390   4.167 3.69e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5062 on 450 degrees of freedom
    ## Multiple R-squared:  0.1561, Adjusted R-squared:  0.1336 
    ## F-statistic: 6.934 on 12 and 450 DF,  p-value: 1.415e-11

``` r
best_model<-lm(score~gender+ethnicity+cls_credits+bty_avg, data=evals)
summary(best_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ gender + ethnicity + cls_credits + bty_avg, 
    ##     data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84613 -0.35225  0.08913  0.38913  0.92237 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.51398    0.10406  33.767  < 2e-16 ***
    ## gendermale             0.14687    0.04903   2.996  0.00289 ** 
    ## ethnicitynot minority  0.20775    0.07183   2.892  0.00401 ** 
    ## cls_creditsone credit  0.58015    0.10546   5.501 6.29e-08 ***
    ## bty_avg                0.08214    0.01580   5.199 3.03e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5119 on 458 degrees of freedom
    ## Multiple R-squared:  0.1216, Adjusted R-squared:  0.114 
    ## F-statistic: 15.86 on 4 and 458 DF,  p-value: 3.644e-12

``` r
#score=3.51398+0.14687male+0.20775(minority)h+0.58015 non-credit+0.08214vty_avg
```

``` r
#numerical:each increase in bty_avg increases 0.08214 in score
#categorical:holding other variables constant, males are 0.14687 higher in the score than females.
```

``` r
#male, minority, one-credit, and higher beauty score.
```

``` r
#not very comfortable. does not know the course content and demographic of students in other universities so could not generalize the conclusion.s
```

Add exercise headings as needed.
