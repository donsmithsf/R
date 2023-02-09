------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 15” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

### Question 1. Accessing data - Download the folder Bike-Sharing-Dataset.zip from this link - <https://archive.ics.uci.edu/ml/machine-learning-databases/00275/>.

##### The folder will contain two datafiles hour.csv and day.csv. You will use day.csv for this assignment. The readme file in the folder has a description of the data, which you are encouraged to read so that you can successfully interpret the analytic results.

##### There are 16 columns in the dataset. You will need to use columns: dteday, temp, and cnt. “cnt” is the outcome variable, or dependent variable.

``` r
df <- read.csv("C:\\Users\\Owner\\Documents\\GitHub\\r\\class assignments\\Data Analytics in R\\Assignment 15\\data\\day.csv")

head(df)
```

    ##   instant     dteday season yr mnth holiday weekday workingday weathersit
    ## 1       1 2011-01-01      1  0    1       0       6          0          2
    ## 2       2 2011-01-02      1  0    1       0       0          0          2
    ## 3       3 2011-01-03      1  0    1       0       1          1          1
    ## 4       4 2011-01-04      1  0    1       0       2          1          1
    ## 5       5 2011-01-05      1  0    1       0       3          1          1
    ## 6       6 2011-01-06      1  0    1       0       4          1          1
    ##       temp    atemp      hum windspeed casual registered  cnt
    ## 1 0.344167 0.363625 0.805833 0.1604460    331        654  985
    ## 2 0.363478 0.353739 0.696087 0.2485390    131        670  801
    ## 3 0.196364 0.189405 0.437273 0.2483090    120       1229 1349
    ## 4 0.200000 0.212122 0.590435 0.1602960    108       1454 1562
    ## 5 0.226957 0.229270 0.436957 0.1869000     82       1518 1600
    ## 6 0.204348 0.233209 0.518261 0.0895652     88       1518 1606

### Question 2. Presenting data – Create an .rmd file in RStudio. Use a code chunk to report a summary of the data.

``` r
summary(df)
```

    ##     instant         dteday              season            yr        
    ##  Min.   :  1.0   Length:731         Min.   :1.000   Min.   :0.0000  
    ##  1st Qu.:183.5   Class :character   1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :366.0   Mode  :character   Median :3.000   Median :1.0000  
    ##  Mean   :366.0                      Mean   :2.497   Mean   :0.5007  
    ##  3rd Qu.:548.5                      3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :731.0                      Max.   :4.000   Max.   :1.0000  
    ##       mnth          holiday           weekday        workingday   
    ##  Min.   : 1.00   Min.   :0.00000   Min.   :0.000   Min.   :0.000  
    ##  1st Qu.: 4.00   1st Qu.:0.00000   1st Qu.:1.000   1st Qu.:0.000  
    ##  Median : 7.00   Median :0.00000   Median :3.000   Median :1.000  
    ##  Mean   : 6.52   Mean   :0.02873   Mean   :2.997   Mean   :0.684  
    ##  3rd Qu.:10.00   3rd Qu.:0.00000   3rd Qu.:5.000   3rd Qu.:1.000  
    ##  Max.   :12.00   Max.   :1.00000   Max.   :6.000   Max.   :1.000  
    ##    weathersit         temp             atemp              hum        
    ##  Min.   :1.000   Min.   :0.05913   Min.   :0.07907   Min.   :0.0000  
    ##  1st Qu.:1.000   1st Qu.:0.33708   1st Qu.:0.33784   1st Qu.:0.5200  
    ##  Median :1.000   Median :0.49833   Median :0.48673   Median :0.6267  
    ##  Mean   :1.395   Mean   :0.49538   Mean   :0.47435   Mean   :0.6279  
    ##  3rd Qu.:2.000   3rd Qu.:0.65542   3rd Qu.:0.60860   3rd Qu.:0.7302  
    ##  Max.   :3.000   Max.   :0.86167   Max.   :0.84090   Max.   :0.9725  
    ##    windspeed           casual         registered        cnt      
    ##  Min.   :0.02239   Min.   :   2.0   Min.   :  20   Min.   :  22  
    ##  1st Qu.:0.13495   1st Qu.: 315.5   1st Qu.:2497   1st Qu.:3152  
    ##  Median :0.18097   Median : 713.0   Median :3662   Median :4548  
    ##  Mean   :0.19049   Mean   : 848.2   Mean   :3656   Mean   :4504  
    ##  3rd Qu.:0.23321   3rd Qu.:1096.0   3rd Qu.:4776   3rd Qu.:5956  
    ##  Max.   :0.50746   Max.   :3410.0   Max.   :6946   Max.   :8714

### Question 3. Preparing data - Extract the month names from the dteday column using lubridate package and save them in a new column month_name, which has a chr data type.

``` r
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
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(jtools)
library(ggstance)
```

    ## 
    ## Attaching package: 'ggstance'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

``` r
library(huxtable)
```

    ## 
    ## Attaching package: 'huxtable'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     add_rownames
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     theme_grey

``` r
df <- df %>% mutate(dteday = as_date(dteday), month_name = as.character(month(dteday, label = TRUE)))
```

### Question 4a. Model 1:

##### a) Use a code chunk to run a simple linear regression model where the dependent variable is cnt and the independent variable is month_name and save the model as Model1.

##### b) Use a code chunk to report the summary for Model1. Below the code chunk, use regular text to comment on the R-squared.

##### c) From the summary of Model1, identify which month is set as a reference. Use regular text (outside of a code chunk) to report the reference month’s predicted cnt.

##### d) With either a code chunk or regular text, use the coefficient estimates from Model1 to report the predicted cnt for the months of January and June.

``` r
model1 <- lm(cnt~month_name, data=df)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ month_name, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5177.2 -1095.2  -249.3  1290.0  4669.7 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     4484.9      196.7  22.799  < 2e-16 ***
    ## month_nameAug   1179.5      275.9   4.275 2.17e-05 ***
    ## month_nameDec  -1081.1      275.9  -3.918 9.79e-05 ***
    ## month_nameFeb  -1829.6      281.8  -6.492 1.58e-10 ***
    ## month_nameJan  -2308.6      275.9  -8.366 3.09e-16 ***
    ## month_nameJul   1078.8      275.9   3.909 0.000101 ***
    ## month_nameJun   1287.5      278.2   4.628 4.38e-06 ***
    ## month_nameMar   -792.6      275.9  -2.873 0.004192 ** 
    ## month_nameMay    864.9      275.9   3.134 0.001793 ** 
    ## month_nameNov   -237.7      278.2  -0.854 0.393113    
    ## month_nameOct    714.3      275.9   2.589 0.009829 ** 
    ## month_nameSep   1281.6      278.2   4.607 4.83e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1524 on 719 degrees of freedom
    ## Multiple R-squared:  0.3906, Adjusted R-squared:  0.3813 
    ## F-statistic:  41.9 on 11 and 719 DF,  p-value: < 2.2e-16

##### b. The r-squared is .39, meaning that this model reports 39% of month_name is predictable from cnt.

##### c. April is missing from the intercept column, therefore it is the reference month and cnt is the y-intercept. The predicted cnt is 4484.9.

##### d. cnt(Jan) = 4484.9-2308.6\*month_name

##### cnt(Jun) = 4484.9+1287.5\*month_name

### Question 4b. Model 2:

##### a) Use a code chunk to run a multiple linear regression model where the dependent variable is cnt and the independent variables are temp and month_name. Save the model as Model2.

##### b) Use a code chunk to report the summary for Model2. Below the code chunk use regular text to comment on the R-squared. Please explain why the R-squared is different from the two simple regression models.

##### c) Compare the coefficient estimates for the month_name Jan variable in Model1 and Model2. With regular text explain why the coefficient estimates are different.

##### d) With either a code chunk or regular text, use the coefficient estimates from Model2 to report the predicted cnt for the month of January when the temperature is .25.

``` r
model2 <- lm(cnt~temp +month_name, data=df)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ temp + month_name, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4896.6 -1080.0  -228.4  1245.2  3372.9 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1554.39     390.76   3.978 7.66e-05 ***
    ## temp           6235.14     729.40   8.548  < 2e-16 ***
    ## month_nameAug  -308.08     315.42  -0.977   0.3290    
    ## month_nameDec  -170.96     283.80  -0.602   0.5471    
    ## month_nameFeb  -764.81     296.15  -2.582   0.0100 *  
    ## month_nameJan  -852.31     313.41  -2.719   0.0067 ** 
    ## month_nameJul  -701.18     335.50  -2.090   0.0370 *  
    ## month_nameJun   -47.47     307.78  -0.154   0.8775    
    ## month_nameMar  -297.20     269.38  -1.103   0.2703    
    ## month_nameMay    86.73     278.37   0.312   0.7555    
    ## month_nameNov   390.66     275.22   1.419   0.1562    
    ## month_nameOct   620.72     263.30   2.357   0.0187 *  
    ## month_nameSep   368.25     285.93   1.288   0.1982    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1453 on 718 degrees of freedom
    ## Multiple R-squared:  0.4469, Adjusted R-squared:  0.4377 
    ## F-statistic: 48.35 on 12 and 718 DF,  p-value: < 2.2e-16

``` r
export_summs(model1,model2)
```

    ## Warning in knit_print.huxtable(x, ...): Unrecognized output format "gfm+tex". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

                 ──────────────────────────────────────────────
                                     Model 1        Model 2    
                                 ──────────────────────────────
                   (Intercept)      4484.90 ***   1554.39 ***  
                                    (196.71)      (390.76)     
                   month_nameAug    1179.52 ***   -308.08      
                                    (275.94)      (315.42)     
                   month_nameDec   -1081.09 ***   -170.96      
                                    (275.94)      (283.80)     
                   month_nameFeb   -1829.60 ***   -764.81 *    
                                    (281.83)      (296.15)     
                   month_nameJan   -2308.56 ***   -852.31 **   
                                    (275.94)      (313.41)     
                   month_nameJul    1078.78 ***   -701.18 *    
                                    (275.94)      (335.50)     
                   month_nameJun    1287.47 ***    -47.47      
                                    (278.19)      (307.78)     
                   month_nameMar    -792.64 **    -297.20      
                                    (275.94)      (269.38)     
                   month_nameMay     864.87 **      86.73      
                                    (275.94)      (278.37)     
                   month_nameNov    -237.72        390.66      
                                    (278.19)      (275.22)     
                   month_nameOct     714.33 **     620.72 *    
                                    (275.94)      (263.30)     
                   month_nameSep    1281.62 ***    368.25      
                                    (278.19)      (285.93)     
                   temp                           6235.14 ***  
                                                  (729.40)     
                                 ──────────────────────────────
                   N                 731           731         
                   R2                  0.39          0.45      
                 ──────────────────────────────────────────────
                   *** p < 0.001; ** p < 0.01; * p <           
                   0.05.                                       

Column names: names, Model 1, Model 2

``` r
cnt_jan = 4484.9+ 6235.14*.25+1287.5*-852.31
cnt_jan
```

    ## [1] -1091305

##### b. The r-squared for model 2 is .15, which means model 2 reports 15% of the variation in cnt from independant variables temp and month_name. The percentage increased from 10% because of the additional variable which was added to the model, which made it more accurate at predicting variation in the data. These models together (.10 + .15) explain .25 of the variation in the data.

##### c. Coefficient estimates represent the unique effect of each predictor variable. January for Model2 is -852.31, while Jan for Model1 is -2308.6. This difference shows that these coefficients are correlated with each other.

##### d. cnt(Jan): -1091305
