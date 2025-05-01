## 1. An analogue signal received at a detector, measured in microvolts, is normally distributed with a mean of 200 and variance of 256.

### Given variables:

    mu <- 200
    vu <- sqrt(256)

1.  What is the probability that the signal will exceed 224 *μ*V?

<!-- -->

    p_exceed <- 1 - pnorm(224, mu, vu)
    p_exceed

    ## [1] 0.0668072

1.  What is the probability that it will be between 186 and 224 *μ*V?

<!-- -->

    p_between <- pnorm(224, mu, vu) - pnorm(186, mu, vu)
    p_between

    ## [1] 0.7424058

1.  What is the micro voltage below which 25% of the signals will be?

<!-- -->

    v25_percent <- qnorm(0.25, mu, vu)
    v25_percent

    ## [1] 189.2082

1.  What is the probability that the signal will be less than 240 *μ*V,
    given that it is larger than 210 *μ*V?

<!-- -->

    numerator_240 <- pnorm(240, mu, vu) - pnorm(210, mu, vu)
    denominator_240 <- 1 - pnorm(210, mu, vu)
    cond_prob_240 <- numerator_240 / denominator_240
    cond_prob_240

    ## [1] 0.9766541

1.  Estimate the interquartile range.

<!-- -->

    Q1 <- qnorm(0.25, mu, vu)
    Q3 <- qnorm(0.75, mu, vu)
    IQR <- Q3 - Q1
    IQR

    ## [1] 21.58367

1.  What is the probability that the signal will be less than 220 *μ*V,
    given that it is larger than 210 *μ*V?

<!-- -->

    numerator_220 <- pnorm(220, mu, vu) - pnorm(210, mu, vu)
    denominator_220 <- 1 - pnorm(210, mu, vu)
    cond_prob_220 <- numerator_220 / denominator_220
    cond_prob_220

    ## [1] 0.6027988

1.  If we know that a received signal is greater than 200 *μ*V, what is
    the probability that it is in fact greater than 220 *μ*V?

<!-- -->

    numerator_200 <- 1 - pnorm(220, mu, vu)
    denominator_200 <- 1 - pnorm(200, mu, vu)
    cond_prob_200 <- numerator_200 / denominator_200
    cond_prob_200

    ## [1] 0.2112995

## 2. A manufacturer of a particular type of computer system is interested in improving its customer support services. Over a period of six months, customers were surveyed and the amount of downtime (in minutes) due to system failures was recorded.

### Given variables

    md <- 25
    vd <- sqrt(144)

1.  Obtain bounds which will include 95% of the downtime of all the
    customers

<!-- -->

    lower_bound <- qnorm(0.025, md, vd)
    upper_bound <- qnorm(0.975, md, vd)
    cat("The lower bound is",lower_bound,"\nThe upper bound is", upper_bound)

    ## The lower bound is 1.480432 
    ## The upper bound is 48.51957

1.  Obtain the bound above which 10% of the downtime is included

<!-- -->

    downtime_90th_percentile <- qnorm(0.90, md, vd)
    downtime_90th_percentile

    ## [1] 40.37862
