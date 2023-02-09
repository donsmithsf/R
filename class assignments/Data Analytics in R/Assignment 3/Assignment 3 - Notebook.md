------------------------------------------------------------------------

title: “Data Analytics in R - Assignment 3” author: “Don Smith” output:
github_document

------------------------------------------------------------------------

# Questions 1 and 2: SAT Testing

#### The SAT is a standardized college admissions test used in the United States. The following two multi-part questions will ask you some questions about SAT testing.

#### This is a 6-part question asking you to determine some probabilities of what happens when a student guessed for all of their answers on the SAT. Use the information below to inform your answers for the following questions.

#### An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

## Question 1a

#### What is the probability of guessing correctly for one question?

``` r
p <- 1/5 # one correct choice of 5 options
p
```

    ## [1] 0.2

## Question 1b

#### What is the expected value of points for guessing on one question? (Hint: Formula for expected value of a random variable.)

``` r
a <- 1
b <- -.25
mu <- a*p + b*(1-p)
mu
```

    ## [1] 0

## Question 1c

#### What is the expected score of guessing on all 44 questions?

``` r
n <- 44
```

#### Expected value of the sum of n draws of a random variable

``` r
ev <- n*mu
ev
```

    ## [1] 0

## Question 1d

#### What is the standard error of guessing on all 44 questions?

``` r
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p*(1 - p))
se <- sqrt(n) * abs(b - a) * sqrt(p*(1 - p))
se
```

    ## [1] 3.316625

## Question 1e

#### Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.

``` r
1-pnorm(8, mu, se)
```

    ## [1] 0.007930666

## Question 1f

#### Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test. What is the probability that a guessing student scores 8 points or higher?

``` r
set.seed(21, sample.kind = "Rounding")
```

    ## Warning in set.seed(21, sample.kind = "Rounding"): non-uniform 'Rounding'
    ## sampler used

``` r
B <- 10000
X <- replicate(B, {
  
  y <- sample(c(a, b), size = n, replace = TRUE, prob=c(p,1-p))
  sum(y)
})

sum(X >=8)/B
```

    ## [1] 0.008

# Question 2

#### The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.

#### In this two-part question, you’ll explore how that affected the expected values for the test.

## Question 2a

#### Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.

#### What is the expected value of the score when guessing on this new test?

``` r
p <- 1/4
a <- 1
b <- 0
mu <- n* a*p + b*(1-p)
mu
```

    ## [1] 11

## Question 2b

#### Consider a range of correct answer probabilities p \<- seq(0.25, 0.95, 0.05) representing a range of student skills.

#### What is the lowest p such that the probability of scoring over 35 exceeds 80%?

``` r
p <- seq(0.25, 0.95, 0.05)

fu <- sapply(p, function(p){
  # calculate the expected value at given p
  expected_value <- n * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(n) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
})


min(p[which(fu > 0.8)])
```

    ## [1] 0.85
