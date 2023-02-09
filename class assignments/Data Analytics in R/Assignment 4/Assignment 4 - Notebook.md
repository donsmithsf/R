------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 4” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

# Esophageal cancer and alcohol/tobacco use

``` r
#Load the tidyverse package
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

#### Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).

#### The dataset is available in base R and can be called with the variable name esoph:

``` r
head(esoph)
```

    ##   agegp     alcgp    tobgp ncases ncontrols
    ## 1 25-34 0-39g/day 0-9g/day      0        40
    ## 2 25-34 0-39g/day    10-19      0        10
    ## 3 25-34 0-39g/day    20-29      0         6
    ## 4 25-34 0-39g/day      30+      0         5
    ## 5 25-34     40-79 0-9g/day      0        27
    ## 6 25-34     40-79    10-19      0         7

#### Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.

## Question 1a

#### How many groups are in the study?

``` r
nrow(esoph)
```

    ## [1] 88

## Question 1b

#### How many cases are there?

``` r
all_cases <- sum(esoph$ncases)
print(all_cases)
```

    ## [1] 200

## Question 1c

#### How many controls are there?

``` r
all_controls <- sum(esoph$ncontrols)
print(all_controls)
```

    ## [1] 775

## Question 2a

#### What is the probability that a subject in the highest alcohol consumption group is a cancer case?

``` r
t <- unique(esoph$alcgp)
print(t)
```

    ## [1] 0-39g/day 40-79     80-119    120+     
    ## Levels: 0-39g/day < 40-79 < 80-119 < 120+

``` r
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)
```

    ##   sum_cases tot probability
    ## 1        45  67   0.6716418

## Question 2b

#### What is the probability that a subject in the lowest alcohol consumption group is a cancer case?

``` r
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
```

    ## [1] 0.06987952

## Question 2c

#### Given that a person is a case, what is the probability that they smoke 10g or more a day?

``` r
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases
```

    ## [1] 0.61

## Question 2d

#### Given that a person is a control, what is the probability that they smoke 10g or more a day?

``` r
tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_controls/all_controls
```

    ## [1] 0.4232258

## Question 3a

#### For cases, what is the probability of being in the highest alcohol group?

``` r
high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc
```

    ## [1] 0.225

## Question 3b

#### For cases, what is the probability of being in the highest tobacco group?

``` r
high_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_tob <- high_tob_cases/all_cases
p_case_high_tob
```

    ## [1] 0.155

## Question 3c

#### For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?

``` r
high_alc_tob_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc_tob <- high_alc_tob_cases/all_cases
p_case_high_alc_tob
```

    ## [1] 0.05

## Question 3d

#### For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?

``` r
p_case_either_highest <- p_case_high_alc + p_case_high_tob - p_case_high_alc_tob
p_case_either_highest
```

    ## [1] 0.33

## Question 4a

#### For controls, what is the probability of being in the highest alcohol group?

``` r
high_alc_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc <- high_alc_controls/all_controls
p_control_high_alc
```

    ## [1] 0.0283871

## Question 4b

#### How many times more likely are cases than controls to be in the highest alcohol group?

``` r
p_case_high_alc/p_control_high_alc
```

    ## [1] 7.926136

## Question 4c

#### For controls, what is the probability of being in the highest tobacco group?

``` r
high_tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_tob <- high_tob_controls/all_controls
p_control_high_tob
```

    ## [1] 0.06580645

## Question 4d

#### For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?

``` r
high_alc_tob_controls <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc_tob <- high_alc_tob_controls/all_controls
p_control_high_alc_tob
```

    ## [1] 0.003870968

## Question 4e

#### For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?

``` r
p_control_either_highest <- p_control_high_alc + p_control_high_tob - p_control_high_alc_tob
p_control_either_highest
```

    ## [1] 0.09032258

## Question 4f

#### How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?

``` r
p_case_either_highest/p_control_either_highest
```

    ## [1] 3.653571
