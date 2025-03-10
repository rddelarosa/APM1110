## I. Geometric Distribution. Provide an R code for the geometric distribution. The geometric distribution is a probability distribution that models the number of trials required to achieve the first success in a sequence of Bernoulli trials, where each trial has a constant probability of success.

1.  Set the probability of success: `p <- 0.2`
2.  Generate 1000 random variables from the geometric distribution.
3.  Calculate basic statistics:
    -   `mean_x <- mean(x)`
    -   `var_x <- var(x)`
    -   `sd_x <- sd(x)`
4.  Print the results with the following format:
    -   Number of trials required to achieve first success:
    -   Mean (in 2 decimal places):
    -   Variance (in 2 decimal places):
    -   Standard deviation (in 2 decimal places):
5.  Plot the histogram of the results.

<!-- -->

    # Set probability of success
    p <- 0.2

    # Generate 1000 random variables from geometric distribution
    x <- rgeom(1000, p)  

    # Calculate statistics
    mean_x <- mean(x)
    var_x <- var(x)
    sd_x <- sd(x)

    # Print results
    cat("Number of trials required to achieve first success:\n Mean (in 2 decimal places):", round(mean_x, 2),"\n Variance (in 2 decimal places):", round(var_x, 2), "\n Standard deviation (in 2 decimal places):", round(sd_x, 2))

    ## Number of trials required to achieve first success:
    ##  Mean (in 2 decimal places): 4.21 
    ##  Variance (in 2 decimal places): 23.45 
    ##  Standard deviation (in 2 decimal places): 4.84

    # Plot histogram
    hist(x, col = "blue", main = "Histogram of Geometric Distribution", xlab = "Number of Trials", ylab = "Frequency", border = "black")

![](SEC_1-FA6_GROUP_7-DELA-ROSA,-R_files/figure-markdown_strict/geometric_distribution-1.png)

## II. Hypergeometric Distribution. Consider a plant manufacturing IC chips of which 10% are expected to be defective. The chips are packed in boxes for export. Before transportation, a sample is drawn from each box. Estimate the probability that the sample contains more than 10% defectives, when:

1.  A sample of 10 is selected from a box of 40;
2.  A sample of 10 is selected from a box of 5000.

<!-- -->

    # Case 1: Sample of 10 from a box of 40
    N <- 40   # Population
    K <- 10   # Sample size
    m <- round(N * 0.1)  # Total defective items 
    n <- N - m  # Total non-defective items
    x <- ceiling(K * 0.1) + 1  # More than 10% defectives in the sample

    # Calculate probability
    p_case1 <- sum(dhyper(x:K, m, n, K))
    cat("Probability of more than 10% defectives in a box of 40 with 10 sampled:",p_case1,"or", round((100*p_case1),2),"%")

    ## Probability of more than 10% defectives in a box of 40 with 10 sampled: 0.2558814 or 25.59 %

    # Case 2: Sample of 10 from a box of 5000
    N <- 5000  # Population
    K <- 10    # Sample size
    m <- round(N * 0.1)  # Total defective items
    n <- N - m  # Total non-defective items
    x <- ceiling(K * 0.1) + 1  # More than 10% defectives in the sample

    # Calculate probability
    p_case2 <- sum(dhyper(x:K, m, n, K))
    cat("Probability of more than 10% defectives in a box of 5000 with 10 sampled:",p_case2,"or",round((100*p_case2),2),"%")

    ## Probability of more than 10% defectives in a box of 5000 with 10 sampled: 0.2638622 or 26.39 %
