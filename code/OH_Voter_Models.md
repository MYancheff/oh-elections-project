Models
================

``` r
m12 <- glm(CCESminusVoterReg ~ age + I(age^2), data = diff12, family = gaussian())
summary(m12)
```

    ## 
    ## Call:
    ## glm(formula = CCESminusVoterReg ~ age + I(age^2), family = gaussian(), 
    ##     data = diff12)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.39970  -0.05698   0.02266   0.06021   0.17119  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.636e-01  1.049e-01   5.373 1.12e-06 ***
    ## age         -1.312e-02  4.003e-03  -3.277  0.00169 ** 
    ## I(age^2)     8.386e-05  3.501e-05   2.396  0.01948 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.009887736)
    ## 
    ##     Null deviance: 1.0470  on 67  degrees of freedom
    ## Residual deviance: 0.6427  on 65  degrees of freedom
    ## AIC: -116.01
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
m14 <- glm(CCESminusVoterReg ~ age+ I(age^2), data = diff14, family = gaussian())
summary(m14)
```

    ## 
    ## Call:
    ## glm(formula = CCESminusVoterReg ~ age + I(age^2), family = gaussian(), 
    ##     data = diff14)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.57487  -0.06211   0.02481   0.06792   0.44698  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  3.500e-01  1.360e-01   2.573   0.0123 *
    ## age         -1.178e-02  5.310e-03  -2.218   0.0299 *
    ## I(age^2)     9.726e-05  4.717e-05   2.062   0.0431 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.02100346)
    ## 
    ##     Null deviance: 1.5236  on 69  degrees of freedom
    ## Residual deviance: 1.4072  on 67  degrees of freedom
    ## AIC: -66.83
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
m16 <- glm(CCESminusVoterReg ~ age+ I(age^2), data = diff16, family = gaussian())
summary(m16)
```

    ## 
    ## Call:
    ## glm(formula = CCESminusVoterReg ~ age + I(age^2), family = gaussian(), 
    ##     data = diff16)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.54754  -0.02255   0.00928   0.05655   0.23883  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  2.928e-01  9.629e-02   3.040  0.00332 **
    ## age         -4.034e-03  3.825e-03  -1.055  0.29518   
    ## I(age^2)     2.376e-05  3.427e-05   0.693  0.49054   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01351651)
    ## 
    ##     Null deviance: 1.01812  on 72  degrees of freedom
    ## Residual deviance: 0.94616  on 70  degrees of freedom
    ## AIC: -102.08
    ## 
    ## Number of Fisher Scoring iterations: 2
