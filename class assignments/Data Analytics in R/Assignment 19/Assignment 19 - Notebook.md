------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 19” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

# Part 1: Code

### 1. Read in data.

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2
    ## ──

    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.3     ✔ forcats 1.0.0
    ## ✔ purrr   1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ purrr::lift()   masks caret::lift()

``` r
# import dataset
df <- readRDS("C:\\Users\\Owner\\Documents\\GitHub\\r\\class assignments\\Data Analytics in R\\Assignment 19\\data\\mod6HE_logit.rds")

head(df)
```

    ## # A tibble: 6 × 17
    ## # Groups:   store [1]
    ##   store  week high_med_gp high_me…¹ high_…² high_…³  size region promo…⁴ altbe…⁵
    ##   <fct> <dbl>       <dbl>     <dbl>   <dbl>   <dbl> <int> <fct>    <dbl>   <dbl>
    ## 1 186       1           1         1       1       1   966 ONTAR…   0.219   0.249
    ## 2 186       2           1         1       1       0   966 ONTAR…   0.297   0.271
    ## 3 186       3           1         1       1       0   966 ONTAR…   0.295   0.276
    ## 4 186       4           1         1       1       0   966 ONTAR…   0.296   0.288
    ## 5 186       5           1         1       1       0   966 ONTAR…   0.290   0.293
    ## 6 186       6           1         1       1       0   966 ONTAR…   0.313   0.282
    ## # … with 7 more variables: confect_units_per <dbl>, salty_units_per <dbl>,
    ## #   velocityA_units_per <dbl>, velocityB_units_per <dbl>,
    ## #   velocityC_units_per <dbl>, velocityD_units_per <dbl>,
    ## #   velocityNEW_units_per <dbl>, and abbreviated variable names ¹​high_med_rev,
    ## #   ²​high_med_units, ³​high_med_gpm, ⁴​promo_units_per, ⁵​altbev_units_per

``` r
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

### 2. Initial loading of data, packages, and functions.

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

### 3. Prepare data.

``` r
# Not for the model (for use later)
logit1 <- df %>% 
  ungroup() %>% 
  select(store, week, high_med_rev, high_med_gp, high_med_gpm) 

# For use in the model
logit2 <- df %>% 
  ungroup() %>% 
  select(high_med_units, 
         size, region, promo_units_per, 
         altbev_units_per, confect_units_per, salty_units_per,
         velocityA_units_per, velocityB_units_per, velocityC_units_per, velocityD_units_per, velocityNEW_units_per)

# Check that "positive" is last for the `my_confusion_matrix` to work 
contrasts(factor(logit2$high_med_units))
```

    ##   1
    ## 0 0
    ## 1 1

### 4. Partition the data into testing and training datasets.

``` r
#install.packages('caret') (don't install twice)
library(caret)
set.seed(77) 
partition <- caret::createDataPartition(y=logit2$high_med_units, p=.75, list=FALSE)
data_train <- logit2[partition, ]
data_test <- logit2[-partition, ]
```

### 5. Train the multivariate model - these are the instructions part of machine learning.

``` r
model_train <- glm(high_med_units ~ ., family=binomial, data=data_train)
summary(model_train)
```

    ## 
    ## Call:
    ## glm(formula = high_med_units ~ ., family = binomial, data = data_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.6429  -0.8146  -0.2296   0.8483   2.6409  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -3.180e+01  3.462e+00  -9.186  < 2e-16 ***
    ## size                   1.932e-02  8.683e-04  22.254  < 2e-16 ***
    ## regionWEST             1.225e+00  7.507e-02  16.320  < 2e-16 ***
    ## regionQUEBEC           1.640e+00  9.517e-02  17.230  < 2e-16 ***
    ## regionATLANTIC         4.150e-01  1.853e-01   2.239  0.02513 *  
    ## promo_units_per       -6.742e+00  5.443e-01 -12.386  < 2e-16 ***
    ## altbev_units_per       7.415e+00  9.723e-01   7.627 2.41e-14 ***
    ## confect_units_per     -2.118e+00  1.101e+00  -1.924  0.05437 .  
    ## salty_units_per        2.805e+01  1.506e+00  18.629  < 2e-16 ***
    ## velocityA_units_per    9.541e+00  3.161e+00   3.018  0.00255 ** 
    ## velocityB_units_per    4.945e+00  3.311e+00   1.494  0.13526    
    ## velocityC_units_per    9.932e+00  3.762e+00   2.640  0.00829 ** 
    ## velocityD_units_per    1.626e+00  3.779e+00   0.430  0.66696    
    ## velocityNEW_units_per -3.173e+01  5.079e+00  -6.247 4.18e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10471.8  on 7553  degrees of freedom
    ## Residual deviance:  7628.6  on 7540  degrees of freedom
    ## AIC: 7656.6
    ## 
    ## Number of Fisher Scoring iterations: 5

### 6. Predict the response variable.

``` r
predict_test <- predict(model_train, newdata=data_test, type='response')
```

### 7. Form table to look at the accuracy of the model.

``` r
table2 <- table(predict_test>.5, data_test$high_med_units) #prediction on left and truth on top
my_confusion_matrix(table2)
```

    ##        
    ##           0   1
    ##   FALSE 904 299
    ##   TRUE  339 975

    ## [[1]]
    ## [1] "975 = True Positive (TP), Hit"
    ## 
    ## [[2]]
    ## [1] "904 = True Negative (TN), Rejection"
    ## 
    ## [[3]]
    ## [1] "339 = False Positive (FP), Type 1 Error"
    ## 
    ## [[4]]
    ## [1] "299 = False Negative (FN), Type 2 Error"
    ## 
    ## [[5]]
    ## [1] "0.7465 = Accuracy (TP+TN/(TP+TN+FP+FN))"
    ## 
    ## [[6]]
    ## [1] "0.7653 = Sensitivity, Recall, Hit Rate, True Positive Rate (How many positives did the model get right? TP/(TP+FN))"
    ## 
    ## [[7]]
    ## [1] "0.7273 = Specificity, Selectivity, True Negative Rate (How many negatives did the model get right? TN/(TN+FP))"
    ## 
    ## [[8]]
    ## [1] "0.7420 = Precision, Positive Predictive Value (How good are the model's positive predictions? TP/(TP+FP))"
    ## 
    ## [[9]]
    ## [1] "0.7515 = Negative Predictive Value (How good are the model's negative predictions? TN/(TN+FN)"

### 8. Put the data back together for future use.

``` r
# Put the prediction back into the test data
data_test$prediction <- predict_test

# Create a variable that shows if the prediction was correct 
# (We have to do the classification--in `round(prediction)`--since logistic regression gives us a probability)
data_test <- data_test %>% mutate(correct_prediction = if_else(round(prediction) == high_med_units, 'correct', 'WRONG!'))

# Add back the original data
temp1 <- logit1[-partition, ]
full_test <- bind_cols(temp1, data_test)

# For viewing in class
full_test <- full_test %>% select(store, week, high_med_units, prediction, correct_prediction, size, region, promo_units_per, salty_units_per) 

slice_sample(full_test, n=10)
```

    ## # A tibble: 10 × 9
    ##    store  week high_med_units prediction correct_…¹  size region promo…² salty…³
    ##    <fct> <dbl>          <dbl>      <dbl> <chr>      <int> <fct>    <dbl>   <dbl>
    ##  1 35061     2              0     0.114  correct      910 ONTAR…   0.286  0.190 
    ##  2 12685    25              0     0.0786 correct      896 ATLAN…   0.463  0.152 
    ##  3 84325     8              0     0.781  WRONG!       900 WEST     0.245  0.223 
    ##  4 63999    34              0     0.498  correct      954 WEST     0.380  0.155 
    ##  5 10411    19              0     0.940  WRONG!      1005 QUEBEC   0.221  0.180 
    ##  6 91361    28              1     0.617  correct      972 WEST     0.475  0.172 
    ##  7 35284     1              0     0.198  correct      904 ONTAR…   0.301  0.191 
    ##  8 872      30              0     0.116  correct      937 ONTAR…   0.463  0.0975
    ##  9 813      16              0     0.295  correct      906 ONTAR…   0.315  0.233 
    ## 10 38938    37              0     0.453  correct      918 WEST     0.329  0.139 
    ## # … with abbreviated variable names ¹​correct_prediction, ²​promo_units_per,
    ## #   ³​salty_units_per

# Part 2: Questions

#### 1.a (0.5 points) What feature/variable has the most negative statistically significant coefficient on the trained model summary?

###### The most negative statistically significant coefficient in the trained model summary is promo_units_per.

#### 1.b (1 point) Does selling a higher proportion of alternative beverages increase, decrease, or neither increase nor decrease the chance of having above median units sold? How do you know this?

###### It increases the chance of having above median units sold. We know this because the coefficient for altbev_units_per is 7.415e+00, which is above “1”, defined by median units sold. Because p value is low, it is statistically significant.

#### 1.c (1 point) Does selling a higher proportion of velocity B units increase, decrease, or neither increase nor decrease the chance of having above median units sold? How do you know this?

###### It does not increase the chance of having above median units sold. Since the p value is 0.13526 (p \< 0.05 means it is statistically significant), this is not statistically significant. A larger (insignificant) p-value suggests that changes in the predictor are not associated with changes in the response.

#### 1.d (0.5 points) Examine the accuracy of the predictions on the test data by answering whether there are more true positives or more true negatives?

###### There are more true positives (975) than true negatives (904).

#### 1.e (1 point) If stores are sorted by the `store` feature in an ascending manner (lowest number first), which is the first store in the `full_test` dataset that has a “WRONG!” prediction?

``` r
head(full_test %>% select(store, correct_prediction), n = 13L)
```

    ## # A tibble: 13 × 2
    ##    store correct_prediction
    ##    <fct> <chr>             
    ##  1 186   correct           
    ##  2 186   correct           
    ##  3 186   correct           
    ##  4 186   correct           
    ##  5 186   correct           
    ##  6 186   correct           
    ##  7 186   correct           
    ##  8 186   correct           
    ##  9 186   correct           
    ## 10 186   correct           
    ## 11 186   correct           
    ## 12 186   correct           
    ## 13 186   WRONG!

###### The first store that has a “WRONG!” prediction is Store 186 (row 13).

#### 2. (1 point) In the model training step, which data—training or testing—do we use and why (that is, explain why we split the data into training and testing subsets)?

###### A training set is partitioned from a data set in order to build a machine learning model. From that same data set, a test set is partitioned to validate the model. Data included in the training set is excluded from the test set. You want to “train” the model on the training set, but then to be sure it is accurate, then run it on the test set to ensure you’re not over- or underfitting the model.

#### 3. (1 point) The feature `region` has changed in the summary of the trained model. Further, only three regions show up in the summary of the model. The reasoning for this is that the `glm()` function automatically recognizes that `region` is a categorical variable (specifically a factor in R). This is discussed in our Coursera content. Thus, the `glm()` function has created “dummy variables” for the levels of `region`. Which level of the variable is not present here but rather accounted for in the intercept term?

###### Coefficients tell us the effect of a particular independent variable on the dependent variable. The glm() function determined that region was a categorical variable, and automatically created dummy variables for each level of category.

###### We now have three categorical variables: regionWEST, regionQUEBEC and regionATLANTIC. However, regionONTARIO is missing. This is because the glm() has included regionONTARIO in the intercept, which is done to avoid over-specifying the model.

###### Therefore, the ONTARIO region does not have an associated dummy variable and is accounted for in the intercept.

#### 4. (1 point) Interpret the confusion matrix using the test / holdout data. Specifically, which of the four measures, Sensitivity, Specificity, Precision, or Negative Predictive Value has the highest value? Write a sentence that translates this value into words. That is, say something that starts like this: “this means this model is good at predicting…”.

###### Sensitivity has the highest value, at 0.7653. This is equal to the amount of true positives the model correctly predicted. This means this model is good at predicting correct positive values.

#### 5. (1 point) Interpret the confusion matrix. Specifically, which of the four measures, Sensitivity, Specificity, Precision, or Negative Predictive Value has the lowest value? Write a sentence that translates this value into words. That is say something that starts like this: “this means this model is not as good at predicting…”.

###### Specificity has the lowest value, at 0.7273. This is equal to the amount of true negatives the model correctly predicted. This means this model is good at predicting correct negative values.

#### 6. (2 points) Interpret the confusion matrix. In NANSE’s business setting, which of these measures does NANSE care about the most, sensitivity, specificity, precision, negative predictive value, or something else? Defend your answer in two or three sentences. There is no correct answer here, but you must successfully defend your answer to get credit.

###### I believe that NANSE would care more about sensitivity because they want to make sure the model is accurately predicting outcomes. While the other categories tell you important facts about the effectiveness of the model, sensitivity tells you the amount of true positives the model correctly predicts. From a business standpoint, effectiveness and accuracy are the most important elements.
