# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(RCurl)
library(rvest)
library(stringr)
library(rjson)
library(dplyr)
library(RSelenium)
library(base64enc)
library(ggplot2)
library(ggrepel)

# set parameters
sourceFlag <- 1
minPts <- 1
exclude <- ""
early <- ""
# early <- c("Sun 12:00PM", "Sun 3:30PM")

# set working directory
setwd("~/OneDrive/R/MLB")

# load functions
source("Scripts/functions.R")

# scoring flags
sFlags <- c("FD", "DK")

# download injury table
source("Scripts/injuries.R")

# get FanGraphs projections
if (sourceFlag == 1) {
  source("Scripts/FanGraphs.R")
}

for (sFlag in sFlags) {
  # site names
  if (sFlag == "FD") {
    site <- "FanDuel"
    posStr <- "Pos"
  } else if (sFlag == "DK") {
    site <- "DraftKings"
    posStr <- c("Pos", "Pos2")
  }
  
  # data sources
  sources <- c("DFC", "FanGraphs", "FantasyPros", "lineupLab", "numberFire", "RotoGrinders", "Rotowire",
               "SwishAnalytics")
  
  # download and save projections from all data sources
  if (sourceFlag == 1) {
    for (i in 1:length(sources)) {
      if (sources[i] != "FanGraphs") {
        source(paste0("Scripts/", sources[i], ".R"))
      }
    }
  }
  # load data frames
  for (i in 1:length(sources)) {
    load(paste0("Data/", sFlag, "/", sources[i], ".RData"))
  }
  
  # join projection sources
  pool <- full_join(rotogrinders, dfc, by = "Player") %>%
    full_join(fangraphs, by = "Player") %>%
    full_join(fantasypros, by = "Player") %>%
    full_join(lineuplab, by = "Player") %>%
    full_join(numberFire, by = "Player") %>%
    full_join(rotowire, by = "Player") %>%
    full_join(swishanalytics, by = "Player")
  # keep only those players who have projections from all sources
  # pool <- na.omit(pool)
  
  # enable secondary position column for DraftKings
  if (sFlag == "FD") {
    dvpStr <- "DvP"
  } else {
    dvpStr <- c("DvP", "DvP2")
    pool$DvP2 <- NA
    # for (i in 1:nrow(pool)) {
    #   if (pool$Pos2[i] != "" & !is.na(pool$Pos2[i])) {
    #     pool$DvP2[i] <- dvp$DvP[dvp$Opp == sub("@", "", pool$Opp[i]) & dvp$Pos == pool$Pos2[i]]
    #   }
    # }
  }
  
  # rearrange columns
  pool <- cbind(pool[, c("Player", posStr, "Team", "Opp", "Hand", "Order", "Salary", sources)],
                pool[, -which(colnames(pool) %in% c("Player", posStr, "Team", "Opp", "Hand", "Order",
                                                    "Salary", sources))])
  
  # mean projected points
  poolMean <- sapply(data.frame(t(pool[, sources])), mean, na.rm = T)
  # standard error of projected points
  poolSE <- sapply(data.frame(t(pool[, sources])), calcSE)
  # store Mean and SE in player pool
  pool$Projection <- round(poolMean, 2)
  pool$SE <- round(poolSE, 2)
  # round some columns to 2 decimal points
  # pool[, c("Deviation", "Floor", "Ceil")] <- sapply(pool[, c("Deviation", "Floor", "Ceil")],
  #                                                   round, digits = 2)
  # rearrange columns
  pool <- cbind(pool[, c("Player", posStr, "Team", "Opp", "Hand", "Order", "Salary",
                         "Projection", "SE", dvpStr)],
                pool[, -which(colnames(pool) %in% c("Player", posStr, "Team", "Opp", "Hand", "Order",
                                                    "Salary", "Projection", "SE", dvpStr))])
  # filter out players with very low projections
  pool <- filter(pool, Projection > minPts)
  # sort pool by player mean
  pool <- arrange(pool, desc(Projection))
  
  # remove players who appear on the injury list
  # pool <- filter(pool, !(Player %in% injuries$Player))
  # remove players from the exclude list
  pool <- filter(pool, !(Player %in% exclude))
  # remove players from early games
  # pool <- filter(pool, !(Tipoff %in% early))
  
  # save player pool
  write.csv(pool, file = paste0("Data/", sFlag, "/projections.csv"), row.names = F)
}