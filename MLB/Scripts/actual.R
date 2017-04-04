# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(stringr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# set working directory
setwd("~/OneDrive/GitHub/DFS/MLB")

# load functions
source("Scripts/functions.R")

# scoring flags
sFlags <- c("FD", "DK")

for (sFlag in sFlags) {
  # site name
  if (sFlag == "FD") {
    site <- "FanDuel"
  } else if (sFlag == "DK") {
    site <- "DraftKings"
  }
    
  # scoring system
  if (sFlag == "FD") {
    sHitters <- data.frame(t(c(3, 6, 9, 12, 3.5, 3.2, 3, 6, 3)))
    sPitchers <- data.frame(t(c(6, 4, -3, 3, 3)))
    names(sPitchers) <- c("W", "QS", "ER", "SO", "IP")
  } else if (sFlag == "DK") {
    sHitters <- data.frame(t(c(3, 5, 8, 10, 2, 2, 2, 5, 2)))
    # sPitchers <- data.frame(t(c(4, -2, 2, 2.25, -0.6, -0.6, -0.6, 2.5, 2.5, 5)))
    # names(sPitchers) <- c("W", "ER", "SO", "IP", "H", "BB", "HBP", "CG", "CGSO", "NH")
    sPitchers <- data.frame(t(c(4, -2, 2, 2.25, -0.6, -0.6, -0.6)))
    names(sPitchers) <- c("W", "ER", "SO", "IP", "H", "BB", "HBP")
  }
  names(sHitters) <- c("1B", "2B", "3B", "HR", "RBI", "R", "BB", "SB", "HBP")
  
  # download previous day's stats from Baseball Reference
  actB <- readHTMLTable("http://www.baseball-reference.com/leagues/daily.fcgi?type=b&dates=yesterday",
                          stringsAsFactors = F)[[1]]
  actP <- readHTMLTable("http://www.baseball-reference.com/leagues/daily.fcgi?type=p&dates=yesterday",
                        stringsAsFactors = F)[[1]]
  
  # change column names
  names(actB)[c(2, 3, 4, 8)] <- c("Player", "GL", "Box", "At")
  names(actP)[c(2, 3, 4, 8)] <- c("Player", "GL", "Box", "At")
  # filter out header rows in the middle of the table
  actB <- filter(actB, Player != "Name")
  actP <- filter(actP, Player != "Name")
  # standardize player names
  actB$Player <- sapply(actB$Player, replaceName)
  actP$Player <- sapply(actP$Player, replaceName)
  # set wins to zero if blank
  actP$W[actP$W == ""] <- 0
  # convert IP from decimal to fraction
  actP$IP <- sub(".1", ".333", actP$IP)
  actP$IP <- sub(".2", ".667", actP$IP)
  # set numeric columns
  actB[, -(1:9)] <- sapply(actB[, -(1:9)], as.numeric)
  actP[, -(1:9)] <- sapply(actP[, -(1:9)], as.numeric)
  
  # calculate singles
  actB[, "1B"] <- actB$H - (actB[, "2B"] + actB[, "3B"] + actB$HR)
  # calculate quality starts
  if (sFlag == "FD") {
    actP$QS <- 0
    actP$QS[actP$IP >= 6 & actP$ER <= 3] <- 1
  }
  
  # select relevant columns
  actB <- actB[, c("Player", names(sHitters))]
  actP <- actP[, c("Player", names(sPitchers))]
  # calculate fantasy points
  actB$Actual <- as.numeric(as.matrix(actB[, -1]) %*% as.numeric(sHitters[names(sHitters)]))
  actP$Actual <- as.numeric(as.matrix(actP[, -1]) %*% as.numeric(sPitchers[names(sPitchers)]))
  
  # only keep fantasy points column
  actB <- actB[, c("Player", "Actual")]
  actP <- actP[, c("Player", "Actual")]
  
  # read projections file
  pool <- read.csv(paste0("Data/", sFlag, "/projections.csv"), stringsAsFactors = F)
  # join projections with actual results
  poolB <- left_join(pool[pool$Pos != "P", ], actB, by = "Player")
  poolP <- left_join(pool[pool$Pos == "P", ], actP, by = "Player")
  pool <- rbind(poolB, poolP)
  # calculate relative player value
  pool$Value <- pool$Projection / (pool$Salary / 1000)
  pool$actValue <- pool$Actual / (pool$Salary / 1000)
  # remove faulty actual results
  # pool <- pool[pool$Actual >=0, ]
  
  # save actual results
  write.csv(pool, file = paste0("Data/", sFlag, "/results.csv"), row.names = F)
}