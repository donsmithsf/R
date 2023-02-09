------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 2” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

# Betting on Roulette

#### A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -\$1 and a successful bet yields \$6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.

#### The following questions ask you to do some calculations related to this scenario.

## Question 1

#### What is the expected value of the payout for one bet?

``` r
a <- 6
b <- -1
n <- 500
p <- 5/38
expected_value <- (a*p + b*(1-p))
expected_value
```

    ## [1] -0.07894737

## Question 2

#### What is the standard error of the payout for one bet?

``` r
standard_error <- abs(b - a) * sqrt(p*(1 - p))
standard_error
```

    ## [1] 2.366227

## Question 3

#### What is the expected value of the average payout over 500 bets?

#### (Remember there is a difference between expected value of the average and expected value of the sum.)

``` r
expected_value
```

    ## [1] -0.07894737

## Question 4

#### What is the standard error of the average payout over 500 bets?

#### Remember there is a difference between the standard error of the average and standard error of the sum.

``` r
standard_error/sqrt(n)
```

    ## [1] 0.1058209

## Question 5

#### What is the expected value of the sum of 500 bets?

``` r
evs <- n * expected_value
evs
```

    ## [1] -39.47368

## Question 6

#### What is the standard error of the sum of 500 bets?

``` r
standard_error_s <- sqrt(n) * standard_error
standard_error_s
```

    ## [1] 52.91045

## Question 7

#### Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, Pr(X≤0).

``` r
pnorm(0, evs, standard_error_s)
```

    ## [1] 0.7721805
