------------------------------------------------------------------------

title: “Logistic Regression (Logit) Template” author: “Don Smith”
output: github_document

------------------------------------------------------------------------

# Part 1: Load and summarize

## Initial loading of data, packages, and functions

``` r
# Run this reusable confusion matrix function (https://en.wikipedia.org/wiki/Confusion_matrix)
my_confusion_matrix <- function(cf_table) {
  true_positive <- cf_table[4]
  true_negative <- cf_table[1]
  false_positive <- cf_table[2]
  false_negative <- cf_table[3]
  accuracy <- (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
  sensitivity_recall <- true_positive / (true_positive + false_negative) 
  specificity_selectivity <- true_negative / (true_negative + false_positive)
  precision <- true_positive / (true_positive + false_positive) 
  neg_pred_value <- true_negative/(true_negative + false_negative)
  print(cf_table)
  my_list <- list(sprintf("%1.0f = True Positive (TP), Hit", true_positive),
                  sprintf("%1.0f = True Negative (TN), Rejection", true_negative),
                  sprintf("%1.0f = False Positive (FP), Type 1 Error", false_positive),
                  sprintf("%1.0f = False Negative (FN), Type 2 Error", false_negative),
                  sprintf("%1.4f = Accuracy (TP+TN/(TP+TN+FP+FN))", accuracy), 
                  sprintf("%1.4f = Sensitivity, Recall, Hit Rate, True Positive Rate (How many positives did the model get right? TP/(TP+FN))", sensitivity_recall),
                  sprintf("%1.4f = Specificity, Selectivity, True Negative Rate (How many negatives did the model get right? TN/(TN+FP))", specificity_selectivity),
                  sprintf("%1.4f = Precision, Positive Predictive Value (How good are the model's positive predictions? TP/(TP+FP))", precision),
                  sprintf("%1.4f = Negative Predictive Value (How good are the model's negative predictions? TN/(TN+FN)", neg_pred_value)
  )
  return(my_list)
}
```

## Install and load packages (don’t install twice)

``` r
# install.packages('tidyverse')
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.1
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.3     ✔ forcats 1.0.0
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
# Load data
df <- read_rds("C:\\Users\\Owner\\Documents\\GitHub\\r\\code snippets\\Logistic Regression (Logit) Template\\data\\mod6HE_logit.rds")

# Explore the data and discuss in PowerPoint
summary(df)
```

    ##      store           week        high_med_gp      high_med_rev   
    ##  186    :  52   Min.   : 1.00   Min.   :0.0000   Min.   :0.0000  
    ##  227    :  52   1st Qu.:13.00   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  233    :  52   Median :26.00   Median :0.0000   Median :0.0000  
    ##  236    :  52   Mean   :26.47   Mean   :0.4997   Mean   :0.4997  
    ##  272    :  52   3rd Qu.:39.00   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  291    :  52   Max.   :52.00   Max.   :1.0000   Max.   :1.0000  
    ##  (Other):9759                                                    
    ##  high_med_units    high_med_gpm         size             region    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   : 890.0   ONTARIO :3120  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 916.0   WEST    :4776  
    ##  Median :0.0000   Median :0.0000   Median : 943.0   QUEBEC  :1863  
    ##  Mean   :0.4995   Mean   :0.4639   Mean   : 949.6   ATLANTIC: 312  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.: 972.0                  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1163.0                  
    ##                                                                    
    ##  promo_units_per  altbev_units_per confect_units_per salty_units_per 
    ##  Min.   :0.1053   Min.   :0.2030   Min.   :0.0000    Min.   :0.0000  
    ##  1st Qu.:0.3046   1st Qu.:0.3575   1st Qu.:0.2349    1st Qu.:0.1520  
    ##  Median :0.3451   Median :0.4019   Median :0.2645    Median :0.1716  
    ##  Mean   :0.3506   Mean   :0.4055   Mean   :0.2706    Mean   :0.1737  
    ##  3rd Qu.:0.3929   3rd Qu.:0.4491   3rd Qu.:0.3012    3rd Qu.:0.1928  
    ##  Max.   :0.5797   Max.   :0.7250   Max.   :0.4741    Max.   :0.3684  
    ##                                                                      
    ##  velocityA_units_per velocityB_units_per velocityC_units_per
    ##  Min.   :0.4429      Min.   :0.1410      Min.   :0.00000    
    ##  1st Qu.:0.5744      1st Qu.:0.2032      1st Qu.:0.07078    
    ##  Median :0.6107      Median :0.2162      Median :0.07994    
    ##  Mean   :0.6086      Mean   :0.2169      Mean   :0.08009    
    ##  3rd Qu.:0.6428      3rd Qu.:0.2300      3rd Qu.:0.08929    
    ##  Max.   :0.7500      Max.   :0.3135      Max.   :0.14865    
    ##                                                             
    ##  velocityD_units_per velocityNEW_units_per
    ##  Min.   :0.00000     Min.   :0.000000     
    ##  1st Qu.:0.05533     1st Qu.:0.000000     
    ##  Median :0.07045     Median :0.000000     
    ##  Mean   :0.07918     Mean   :0.004608     
    ##  3rd Qu.:0.10265     3rd Qu.:0.007963     
    ##  Max.   :0.21637     Max.   :0.055623     
    ## 

# Part 2: Run the Logistic Algorithm

## Prepare the data

``` r
# Not for the model (for use later)
logit1 <- df %>% 
  ungroup() %>% 
  select(store, week, high_med_rev, high_med_units, high_med_gpm)

# For use in the model
logit2 <- df %>% 
  ungroup() %>% 
  select(high_med_gp, 
         size, region, promo_units_per, 
         altbev_units_per, confect_units_per, salty_units_per,
         velocityA_units_per, velocityB_units_per, velocityC_units_per, velocityD_units_per, velocityNEW_units_per)

# Check that "positive" is last for the `my_confusion_matrix` to work 
contrasts(factor(logit2$high_med_gp))
```

    ##   1
    ## 0 0
    ## 1 1

## Partition the data into testing and training datasets

``` r
# install.packages('caret') (don't install twice)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
set.seed(77) 
partition <- caret::createDataPartition(y=logit2$high_med_gp, p=.75, list=FALSE)
data_train <- logit2[partition, ]
data_test <- logit2[-partition, ]
```

## Train the multivariate model - these are the instructions part of machine learning

``` r
model_train <- glm(high_med_gp ~ ., family=binomial, data=data_train)
summary(model_train)
```

    ## 
    ## Call:
    ## glm(formula = high_med_gp ~ ., family = binomial, data = data_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.8270  -0.7510  -0.1803   0.7867   2.7439  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -3.366e+01  3.583e+00  -9.395  < 2e-16 ***
    ## size                   1.895e-02  8.941e-04  21.193  < 2e-16 ***
    ## regionWEST             1.417e+00  7.763e-02  18.256  < 2e-16 ***
    ## regionQUEBEC           1.691e+00  9.788e-02  17.275  < 2e-16 ***
    ## regionATLANTIC         1.004e-01  1.928e-01   0.521  0.60233    
    ## promo_units_per       -8.795e+00  5.697e-01 -15.439  < 2e-16 ***
    ## altbev_units_per       1.039e+01  1.017e+00  10.219  < 2e-16 ***
    ## confect_units_per     -1.504e+00  1.140e+00  -1.320  0.18694    
    ## salty_units_per        3.224e+01  1.592e+00  20.258  < 2e-16 ***
    ## velocityA_units_per    1.035e+01  3.264e+00   3.169  0.00153 ** 
    ## velocityB_units_per    5.603e+00  3.422e+00   1.637  0.10160    
    ## velocityC_units_per    1.034e+01  3.886e+00   2.661  0.00779 ** 
    ## velocityD_units_per    2.627e+00  3.905e+00   0.673  0.50111    
    ## velocityNEW_units_per -3.400e+01  5.258e+00  -6.466 1.01e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10471.9  on 7553  degrees of freedom
    ## Residual deviance:  7228.3  on 7540  degrees of freedom
    ## AIC: 7256.3
    ## 
    ## Number of Fisher Scoring iterations: 5

### Two main takeaways to consider:

1)  What do the coefficients mean here, specifically, for the
    coefficient on “promo_units_per”? Does it help or hurt
    profitability?

2)  What does the p-value mean?

## Predict the response variable (Use the instructions to predict the likelihood of high gross profit)

``` r
predict_test <- predict(model_train, newdata=data_test, type='response')
```

## Form table to look at the accuracy of the model

``` r
table2 <- table(predict_test>.5, data_test$high_med_gp) #prediction on left and truth on top
my_confusion_matrix(table2)
```

    ##        
    ##            0    1
    ##   FALSE  948  263
    ##   TRUE   294 1012

    ## [[1]]
    ## [1] "1012 = True Positive (TP), Hit"
    ## 
    ## [[2]]
    ## [1] "948 = True Negative (TN), Rejection"
    ## 
    ## [[3]]
    ## [1] "294 = False Positive (FP), Type 1 Error"
    ## 
    ## [[4]]
    ## [1] "263 = False Negative (FN), Type 2 Error"
    ## 
    ## [[5]]
    ## [1] "0.7787 = Accuracy (TP+TN/(TP+TN+FP+FN))"
    ## 
    ## [[6]]
    ## [1] "0.7937 = Sensitivity, Recall, Hit Rate, True Positive Rate (How many positives did the model get right? TP/(TP+FN))"
    ## 
    ## [[7]]
    ## [1] "0.7633 = Specificity, Selectivity, True Negative Rate (How many negatives did the model get right? TN/(TN+FP))"
    ## 
    ## [[8]]
    ## [1] "0.7749 = Precision, Positive Predictive Value (How good are the model's positive predictions? TP/(TP+FP))"
    ## 
    ## [[9]]
    ## [1] "0.7828 = Negative Predictive Value (How good are the model's negative predictions? TN/(TN+FN)"

### Questions:

1)  Which (sensitivity, specificity, precision, negative predictive
    value) is the highest, and what does that mean?

2)  For a given use case, which is most important (sensitivity,
    specificity, precision, negative predictive value), or something
    else?

# Part 3: Use the predictions above to help the business

## Put the data back together for future use

``` r
# Put the prediction back into the test data
data_test$prediction <- predict_test

# Create a variable that shows if the prediction was correct 
# (Must do the classification--in `round(prediction)`--since logistic regression gives us a probability)
data_test <- data_test %>% mutate(correct_prediction = if_else(round(prediction) == high_med_gp, 'correct', 'WRONG!'))

# Add back the original data
temp1 <- logit1[-partition, ]
full_test <- bind_cols(temp1, data_test)

# For viewing
full_test <- full_test %>% 
  select(store, week, high_med_gp, prediction, correct_prediction, size, region, promo_units_per, salty_units_per)
slice_sample(full_test, n=10)
```

    ## # A tibble: 10 × 9
    ##    store  week high_med_gp prediction correct_pre…¹  size region promo…² salty…³
    ##    <fct> <dbl>       <dbl>      <dbl> <chr>         <int> <fct>    <dbl>   <dbl>
    ##  1 35061     2           0     0.0989 correct         910 ONTAR…   0.286  0.190 
    ##  2 12685    25           0     0.0411 correct         896 ATLAN…   0.463  0.152 
    ##  3 84325     8           0     0.866  WRONG!          900 WEST     0.245  0.223 
    ##  4 63999    34           0     0.486  correct         954 WEST     0.380  0.155 
    ##  5 10411    19           0     0.957  WRONG!         1005 QUEBEC   0.221  0.180 
    ##  6 91361    28           1     0.590  correct         972 WEST     0.475  0.172 
    ##  7 35284     1           0     0.200  correct         904 ONTAR…   0.301  0.191 
    ##  8 872      30           0     0.0999 correct         937 ONTAR…   0.463  0.0975
    ##  9 813      16           0     0.279  correct         906 ONTAR…   0.315  0.233 
    ## 10 38938    37           0     0.514  WRONG!          918 WEST     0.329  0.139 
    ## # … with abbreviated variable names ¹​correct_prediction, ²​promo_units_per,
    ## #   ³​salty_units_per
