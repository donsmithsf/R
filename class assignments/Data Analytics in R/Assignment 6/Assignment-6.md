------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 6” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

# Question 1: Olympic Running

#### In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

#### Use the information above to help you answer the following four questions.

## Question 1a

#### How many different ways can the 3 medals be distributed across 8 runners?

``` r
library(gtools)

medals <- permutations(8,3)
nrow(medals)
```

    ## [1] 336

## Question 1b

#### How many different ways can the three medals be distributed among the 3 runners from Jamaica?

``` r
# 3 factorial
jamaica <- permutations(3,3)
nrow(jamaica)
```

    ## [1] 6

## Question 1c

#### What is the probability that all 3 medals are won by Jamaica?

``` r
nrow(jamaica)/nrow(medals)
```

    ## [1] 0.01785714

#### 56 total combinations, only one with 1, 2, 3. Consequently, 1/56 = 0.0178.

## Question 1d

#### Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:

#### runners \<- c(“Jamaica”, “Jamaica”, “Jamaica”, “USA”, “Ecuador”, “Netherlands”, “France”, “South Africa”)

#### For each iteration of the Monte Carlo simulation, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop. Then calculate the probability that all the runners are from Jamaica.

``` r
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)
```

    ## [1] 0.0174
