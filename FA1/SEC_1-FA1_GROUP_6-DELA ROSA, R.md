---
title: "SEC1_FA1_GROUP6_DELA_ROSA_R.md"
author: "Roland Dela Rosa"
date: "Feb 1, 2025"
output: md_document
subtitle: "**Github Link: https://github.com/rddelarosa/APM1110/blob/main/FA1/SEC_1-FA1_GROUP_6-DELA%20ROSA%2C%20R.md**"
---

```r
knitr::opts_chunk$set(echo = TRUE)
```

## 1. Write the skewness program, and use it to calculate the skewness coefficient of the four examination subjects in results.txt (results.csv). What can you say about these data?

## Pearson has given an approximate formula for the skewness that is easier to calculate than the exact formula given in Equation 2.1.

### Read the dataset

```r
data <- read.table("results_clean.txt", header = TRUE)
```

### Compute Normal Skewness

```r
skew_n <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)  
  sum1 <- sum((x-mean_x) ** 2, na.rm = TRUE)
  sum2 <- sum((x-mean_x) ** 3, na.rm = TRUE)
  skew <- (sqrt(length(x))* sum2)/(sum1 **(3/2))
  skew
}
```

### Compute Pearson's skewness for each subject

```r
skew_p <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  median_x <- median(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  
  return(3 * (mean_x - median_x) / sd_x)
}

subjects <- c("Arch1", "Prog1", "Arch2", "Prog2")

result <- sapply(subjects, function(subj) {
  list(Skewness_Coefficient = skew_n(data[[subj]]), Pearson = skew_p(data[[subj]]))
})
print(result)
```

### What can you say about these data (Skewness Coefficient)?

In the analysis of the Skewness Coefficient results, Prog1, Arch2, and Prog2 are fairly symmetrical. This shows that the majority of students in Prog1 and Prog2 performed well, with some having low scores. For Arch2, more students scored below average, which explains why a small group of students is excelling. On the other hand, Arch1 is moderately skewed, indicating that more students scored above average.

### Is it a reasonable approximation?

Yes, Pearson’s Skewness provides a reasonable approximation in most cases. In this analysis, comparing Pearson’s Skewness with the Skewness Coefficient for Arch1, Arch2, and Prog2, the difference between Pearson’s and Normal Skewness is small, indicating that Pearson’s offers a reasonable approximation. However, for Prog1, Pearson’s slightly overestimates skewness.

---

## 2. For the class of 50 students of computing detailed in Exercise 1.1, use R to

### (a) Form the stem-and-leaf display for each gender, and discuss the advantages of this representation compared to the traditional histogram.

### (b) Construct a box-plot for each gender and discuss the findings.

### Define the data

```r
females <- c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43, 44, 75, 78, 80, 81, 83, 83, 85)
males <- c(48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56, 42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)
```

### (a) Stem-and-Leaf Display

```r
cat("\nStem-and-Leaf for Females:\n")
stem(females)

cat("\nStem-and-Leaf for Males:\n")
stem(males)
```

### Stem-and-Leaf vs. Histogram

The stem-and-leaf plot preserves individual data while showing the overall shape of the distribution. It provides a more detailed view of the actual values while still displaying the distribution. On the other hand, histograms are better suited for larger datasets where you want to observe the general shape of the distribution.

---

### (b) Boxplot Comparison

```r
boxplot(females, males, names = c("Females", "Males"), 
        col = c("pink", "blue"), 
        main = "Boxplot of Scores by Gender", 
        ylab = "Scores")
```

### Interpretation

The boxplot allows us to visualize the spread, median, and presence of outliers. In the analysis, the median score for females is higher than for males, suggesting that the middle values of the female data are generally greater. Also, the range of the female data is wide, but there aren't many outliers. On the other hand, the male data has one high outlier (86), indicating more variation within this group.
