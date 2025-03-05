## 6. An email message can travel through one of three server routes. The percentage of errors in each of the servers and the percentage of messages that travel through each route are shown in the following table. Assume that the servers are independent.

    # Given

    data <- data.frame(
      Server = c("Server 1", "Server 2", "Server 3"),
      Messages = c(0.40, 0.25, 0.35),
      Error_Rate = c(0.01, 0.02, 0.015)
    )
    data

    ##     Server Messages Error_Rate
    ## 1 Server 1     0.40      0.010
    ## 2 Server 2     0.25      0.020
    ## 3 Server 3     0.35      0.015

    pm1 <- 0.40  # Probability of Server 1
    pm2 <- 0.25  # Probability of Server 2
    pm3 <- 0.35  # Probability of Server 3
    e1 <- 0.01  # Error rate for Server 1
    e2 <- 0.02  # Error rate for Server 2
    e3 <- 0.015 # Error rate for Server 3

### (a) What is the probability of receiving an email containing an error?

    p_error <- pm1 * e1 + pm2 * e2 + pm3 * e3

    cat("The probability of receiving an email containing an error is ", round(p_error,5), "or", round(p_error*100,2),"%." )

    ## The probability of receiving an email containing an error is  0.01425 or 1.43 %.

### (b) What is the probability that a message will arrive without error?

    p_no_error <- 1 - p_error

    cat("The probability that a message will arrive without error is ", round(p_no_error,4), "or", round(p_no_error*100,2),"%." )

    ## The probability that a message will arrive without error is  0.9858 or 98.58 %.

### (c) If a message arrives without error, what is the probability that it was sent through server 1?

    ps1_no_error <- (pm1 * (1 - e1)) / p_no_error


    cat("The probability that a message will arrive without error is", round(ps1_no_error, 4), "or", round(ps1_no_error * 100, 2), "%.")

    ## The probability that a message will arrive without error is 0.4017 or 40.17 %.

## 9. A software company surveyed managers to determine the probability that they would buy a new graphics package that includes three-dimensional graphics. About 20% of office managers were certain that they would not buy the package, 70% claimed that they would buy, and the others were undecided. Of those who said that they would not buy the package, only 10% said that they were interested in upgrading their computer hardware. Of those interested in buying the graphics package, 40% were also interested in upgrading their computer hardware. Of the undecided, 20% were interested in upgrading their computer hardware.

-   Let A denote the intention of not buying, B the intention of buying,
    C the undecided, and G the intention of upgrading the computer
    hardware.

<!-- -->

    # Given
    prob_A <- 0.20  # Probability of not buying
    prob_B <- 0.70  # Probability of buying
    prob_C <- 0.10  # Probability of undecided
    gA <- 0.10  # Probability of upgrading given not buying
    gB <- 0.40  # Probability of upgrading given buying
    gC <- 0.20  # Probability of upgrading given undecided

## (a) Calculate the probability that a manager chosen at random will not upgrade the computer hardware ($P(\overline{G})$)

    prob_not_G <- 1-((prob_A * gA) + (prob_B * gB) + (prob_C * gC))

    cat(" The probability that a manager chosen at random will not upgrade the computer hardware is", round(prob_not_G, 4), "or", round(prob_not_G * 100, 2), "%.")

    ##  The probability that a manager chosen at random will not upgrade the computer hardware is 0.68 or 68 %.

## (b) Explain what is meant by the posterior probability of *B* given *G*, *P*(*B*|*G*).

The posterior probability *P*(*B*|*G*) refers to the likelihood that a
manager will purchase the graphics package, assuming they have already
decided to upgrade their hardware. This helps in understanding how prior
decisions influence subsequent choices.

## (c) Construct a tree diagram and use it to calculate the following probabilities: $P(G), P(B\vert G), P(B\vert \overline{G}), P(C\vert G), P(\overline{C}\vert \overline{G})$

    library(data.tree)

    ## Warning: package 'data.tree' was built under R version 4.4.3

    survey <- data.frame(
      Group = c("A", "B", "C"),
      Probability = c(prob_A, prob_B, prob_C),
      Upgrade = c(gA, gB, gC)
    )

    survey$pathString <- paste("Survey", survey$Group, sep = "/")
    tree <- as.Node(survey)

    print(tree, "Probability", "Upgrade")

    ##   levelName Probability Upgrade
    ## 1    Survey          NA      NA
    ## 2     ¦--A          0.2     0.1
    ## 3     ¦--B          0.7     0.4
    ## 4     °--C          0.1     0.2

    prob_g <- (prob_A * gA) + (prob_B * gB) + (prob_C * gC)
    prob_b_g <- round((gB * prob_B) / prob_g, 4)
    prob_b_neg_g <- round((gA * prob_A) / (1 - prob_g), 4)
    prob_c_g <- round((gC * prob_C) / prob_g, 4)
    prob_neg_c_neg_g <- round(((prob_A * (1 - gA)) + (prob_B * (1 - gB))) / (1 - prob_g), 4)

    cat("P(G) =", round(prob_g * 100, 2), "%\n")

    ## P(G) = 32 %

    cat("P(B|G) =", round(prob_b_g * 100, 2), "%\n")

    ## P(B|G) = 87.5 %

    cat("P(B|-G) =", round(prob_b_neg_g * 100, 2), "%\n")

    ## P(B|-G) = 2.94 %

    cat("P(C|G) =", round(prob_c_g * 100, 2), "%\n")

    ## P(C|G) = 6.25 %

    cat("P(-C|-G) =", round(prob_neg_c_neg_g * 100, 2), "%\n")

    ## P(-C|-G) = 88.24 %

    ## A malicious spyware can infect a computer system through the Internet or through email. The spyware comes through the Internet 70% of the time and 30% of the time, it gets in through email. If it enters via the Internet the anti-virus detector will detect it with probability 0.6, and via email, it is detected with probability 0.8.


    ``` r
    # Given 
    prob_I <- 0.70  # Probability of spyware via Internet
    prob_E <- 0.30  # Probability of spyware via Email
    detect_I <- 0.60  # Detection probability via Internet
    detect_E <- 0.80  # Detection probability via Email

### What is the probability that this spyware infects the system?

    prob_infect <- (prob_I * (1 - detect_I)) + (prob_E * (1 - detect_E))

    cat(" The probability that this spyware infects the system is", round(prob_infect, 4), "or", round(prob_infect * 100, 2), "%.")

    ##  The probability that this spyware infects the system is 0.34 or 34 %.

### If the spyware is detected, what is the probability that it came through the Internet?

    p_detected <- (prob_I * detect_I) + (prob_E * detect_E)
    pI_given_detected <- (prob_I * detect_I) / p_detected

    cat(" The probability that it came through the Internet is", round(pI_given_detected, 4), "or", round(pI_given_detected * 100, 2), "%.")

    ##  The probability that it came through the Internet is 0.6364 or 63.64 %.
