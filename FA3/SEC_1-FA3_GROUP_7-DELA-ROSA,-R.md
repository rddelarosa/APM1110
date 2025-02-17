## 2. A binary communication channel carries data as one of two sets of signals denoted by 0 and 1. Owing to noise, a transmitted 0 is sometimes received as a 1, and a transmitted 1 is sometimes received as a 0. For a given channel, it can be assumed that a transmitted 0 is correctly received with probability 0.95, and a transmitted 1 is correctly received with probability 0.75. Also, 70% of all messages are transmitted as a 0. If a signal is sent, determine the probability that:

    P_0 <- 0.70  
    P_1 <- 0.30  
    P_00 <- 0.95 
    P_11 <- 0.75 
    P_10 <- 1 - P_00 
    P_01 <- 1 - P_11

### (a) Probability of receiving a 1.

    P_R1 <- (P_0 * P_10) + (P_1 * P_11)
    P_R1

    ## [1] 0.26

### (b) a 1 was transmitted given than a 1 was received.

    P_T1_R1 <- (P_1 * P_11) / P_R1
    P_T1_R1

    ## [1] 0.8653846

## 7. There are three employees working at an IT company: Jane, Amy, and Ava, doing 10%, 30%, and 60% of the programming, respectively. 8% of Jane’s work, 5% of Amy’s work, and just 1% of Ava‘s work is in error.

    P_Jane <- 0.10 
    P_Amy <- 0.30 
    P_Ava <- 0.60  
    P_E_Jane <- 0.08
    P_E_Amy <- 0.05 
    P_E_Ava <- 0.01 

### (a) What is the overall percentage of error?

    P_Error <- (P_Jane * P_E_Jane) + (P_Amy * P_E_Amy) + (P_Ava * P_E_Ava)
    cat("Overall Error Probability:", P_Error, "\n")

    ## Overall Error Probability: 0.029

    cat("Overall Error Percentage:", P_Error * 100, "%\n")

    ## Overall Error Percentage: 2.9 %

### (b) If a program is found with an error, who is the most likely person to have written it?

    employee <- c("Jane", "Amy", "Ava")

    P_Jane_Most_E <- (P_Jane * P_E_Jane) / P_Error
    P_Amy_Most_E <- (P_Amy * P_E_Amy) / P_Error
    P_Ava_Most_E <- (P_Ava * P_E_Ava) / P_Error

    P_Most_E_employees <- c(P_Jane_Most_E, P_Amy_Most_E, P_Ava_Most_E)


    most_likely <- which.max(P_Most_E_employees)
    most_likely_employee <- employee[most_likely]
    most_likely_prob <- P_Most_E_employees [most_likely]

    E_table <- data.frame(
      Employee = employee,
      Probability_Error = c(P_Jane_Most_E, P_Amy_Most_E, P_Ava_Most_E)
    )

    print (E_table)

    ##   Employee Probability_Error
    ## 1     Jane         0.2758621
    ## 2      Amy         0.5172414
    ## 3      Ava         0.2068966

    cat("Most likely employee responsible for an error:", most_likely_employee, "\n")

    ## Most likely employee responsible for an error: Amy

    cat("The probability:", most_likely_prob, "\n")

    ## The probability: 0.5172414
