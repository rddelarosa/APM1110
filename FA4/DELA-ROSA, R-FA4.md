## 5. A geospatial analysis system has four sensors supplying images. The percent-age of images supplied by each sensor and the percentage of images relevant to a query are shown in the following table.

    # Given
    sensors <- data.frame(
      Sensor = 1:4,
      Percent_Supply = c(15, 20, 25, 40),
      Percent_Relevant = c(50, 60, 80, 85)
    )
    sensors

    ##   Sensor Percent_Supply Percent_Relevant
    ## 1      1             15               50
    ## 2      2             20               60
    ## 3      3             25               80
    ## 4      4             40               85

## What is the overall percentage of relevant images?

    # Compute
    prob_relevance <- sum(sensors$Percent_Supply * sensors$Percent_Relevant) / sum(sensors$Percent_Supply)

    # Output the result
    cat("The overall percentage of relevant images is ", prob_relevance, "%.")

    ## The overall percentage of relevant images is  73.5 %.

## 6. A fair coin is tossed twice. Let *E*<sub>1</sub> be the event that both tosses have the same outcome, that is *E*<sub>1</sub> = (*H**H*, *T**T*). Let *E*<sub>2</sub> be the event that the first toss is a head, that is, *E*<sub>2</sub> = (*H**H*, *H**T*). Let *E*<sub>3</sub> be the event that the second toss is a head, that it, *E*<sub>3</sub> = (*T**H*, *H**H*). Show that *E*<sub>1</sub>, *E*<sub>2</sub>, and *E*<sub>3</sub> are pairwise independent but not mutually independent.

    # Define the sample space
    sample <- c("HH", "HT", "TH", "TT")

    # Define events
    E1 <- c("HH", "TT")  # Both tosses are the same
    E2 <- c("HH", "HT")  # First toss is a head
    E3 <- c("HH", "TH")  # Second toss is a head

    # Calculate probabilities
    P_E1 <- length(E1) / length(sample)
    P_E2 <- length(E2) / length(sample)
    P_E3 <- length(E3) / length(sample)
    P_E1_E2 <- length(intersect(E1, E2)) / length(sample)
    P_E1_E3 <- length(intersect(E1, E3)) / length(sample)
    P_E2_E3 <- length(intersect(E2, E3)) / length(sample)
    P_E1_E2_E3 <- length(intersect(intersect(E1, E2), E3)) / length(sample)

    # Check for pairwise independence
    pairwise_independent <- (P_E1_E2 == P_E1 * P_E2) &
                             (P_E1_E3 == P_E1 * P_E3) &
                             (P_E2_E3 == P_E2 * P_E3)

    # Check for mutual independence
    mutual_independent <- (P_E1_E2_E3 == P_E1 * P_E2 * P_E3)

    # Display results
    cat("P(E1) =", P_E1, "\n")

    ## P(E1) = 0.5

    cat("P(E2) =", P_E2, "\n")

    ## P(E2) = 0.5

    cat("P(E3) =", P_E3, "\n")

    ## P(E3) = 0.5

    cat("P(E1 and E2) =", P_E1_E2, "\n")

    ## P(E1 and E2) = 0.25

    cat("P(E1 and E3) =", P_E1_E3, "\n")

    ## P(E1 and E3) = 0.25

    cat("P(E2 and E3) =", P_E2_E3, "\n")

    ## P(E2 and E3) = 0.25

    cat("P(E1 and E2 and E3) =", P_E1_E2_E3, "\n")

    ## P(E1 and E2 and E3) = 0.25

    cat("Pairwise Independent:", pairwise_independent, "\n")

    ## Pairwise Independent: TRUE

    cat("Mutually Independent:", mutual_independent, "\n")

    ## Mutually Independent: FALSE

    # Conclusion
    cat("\n The events E1, E2, and E3 are pairwise independent but not mutually independent.")

    ## 
    ##  The events E1, E2, and E3 are pairwise independent but not mutually independent.
