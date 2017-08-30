# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("dplyr")
library("stringr")

# set week number
week <- 8

# load actual results
load(paste0("./Data/Week ", week, "/actual.RData"))
# load best lineups
# load(paste0("./Data/Week ", week, "/bestLineups.RData"))
bestLineups <- read.csv(paste0("./Data/Week ", week, "/bestLineups.csv"), header = F)
# load most probable lineup
load(paste0("./Data/Week ", week, "/mostProbable.RData"))

# load functions
source("./Scripts/functions.R")

# actual results
actual <- actual[, 1:3]
names(actual)[3] <- "Actual"
actual$Player <- sapply(actual$Player, replaceName)

# calculate total points scored by each team
lineup <- as.character(t(bestLineups[1, ]))
total <- sum(actual$Actual[actual$Player %in% lineup])
for (i in 2:nrow(bestLineups)) {
  lineup <- as.character(t(bestLineups[i, ]))
  total[i] <- sum(actual$Actual[actual$Player %in% lineup])
}

mostProbable <- left_join(mostProbable, actual, by = c("Player", "Pos"))
mpTotal <- sum(mostProbable$Actual)

# histogram of team point totals
plot(density(total))