# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("GA")

p <- c(6, 5, 8, 9, 6, 7, 3)
w <- c(2, 3, 6, 7, 5, 9, 4)
W <- 9

knapsack <- function(x) {
  f <- sum(x * p)
  penalty <- 100 * abs(sum(x * w) - W)
  f - penalty
}

GA <- ga(type = "binary", fitness = knapsack, nBits = length(w))
summary(GA)