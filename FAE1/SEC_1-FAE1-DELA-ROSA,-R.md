# 13. A malicious spyware can infect a computer system though the Internet or through email. The spyware comes through the Internet 70% of the time and 30% of the time, it gets in through email. If it enters via the Internet the anti-virus detector will detect it with probability 0.6, and via email, it is detected with probability 0.8.

## Given

    P_net <- 0.7
    P_mail <- 0.3
    P_D_net <- 0.6
    P_D_mail <- 0.8

## (a) What is the probability that this spyware infects the system?

    P_Not_D_net <- 1 - P_D_net
    P_Not_D_mail <- 1 - P_D_mail

    P_Infect <- (P_net * P_Not_D_net) + (P_mail * P_Not_D_mail)

    cat("The probability that this spyware infects the system is:", P_Infect)

    ## The probability that this spyware infects the system is: 0.34

## (b) If the spyware is detected, what is the probability that it came through the Internet?

    P_Detect <- (P_net * P_D_net) + (P_mail * P_D_mail)
    P_Internet_given_Detect <- (P_net * P_D_net) / P_Detect

    cat("The probability that the spyware came through the Internet and infects the system is:", P_Internet_given_Detect)

    ## The probability that the spyware came through the Internet and infects the system is: 0.6363636

# 14. Of the emails you receive 20% are spam on average. Your spam filter is able to detect 90% of them but also misclassifies as spam 15% of the genuine emails.

## Given

    P_Spam <- 0.2
    P_Genuine <- 0.8
    P_Detect_Spam <- 0.9
    P_False_Positive <- 0.15

## (a) If an email arrives and is marked spam, what is the probability that it really is spam?

    P_Marked_Spam <- (P_Spam * P_Detect_Spam) + (P_Genuine * P_False_Positive)
    P_Spam_given_Marked <- (P_Spam * P_Detect_Spam) / P_Marked_Spam
    cat("The probability that the email arrives is spam:", P_Spam_given_Marked)

    ## The probability that the email arrives is spam: 0.6

## (b) If an email arrives and is not marked spam, what is the probability that it is legitimate?

    P_Not_Marked_Spam <- (P_Spam * (1 - P_Detect_Spam)) + (P_Genuine * (1 - P_False_Positive))
    P_Genuine_given_Not_Marked <- (P_Genuine * (1 - P_False_Positive)) / P_Not_Marked_Spam

    cat("The probability that the email arrived is legitimate:", P_Genuine_given_Not_Marked)

    ## The probability that the email arrived is legitimate: 0.9714286
