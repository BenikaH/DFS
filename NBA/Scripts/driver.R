# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(RCurl)
library(stringr)
library(rjson)
library(dplyr)
library(RSelenium)
library(ggplot2)
library(ggrepel)

# set parameters
sourceFlag <- 1
minPts <- 15
exclude <- ""
early <- ""
# early <- c("Sun 12:00PM", "Sun 3:30PM")

# set working directory
setwd("~/OneDrive/GitHub/DFS/NBA")

# load functions
source("Scripts/functions.R")

# scoring flags
sFlags <- c("FD", "DK")

# download injury table
source("Scripts/injuries.R")
# read table from Rotogrinders back-to-back tool
source("Scripts/b2b.R")

for (sFlag in sFlags) {
  # site names
  if (sFlag == "FD") {
    site <- "FanDuel"
  } else if (sFlag == "DK") {
    site <- "DraftKings"
  }
  
  # data sources
  sources <- c("FantasyPros", "numberFire", "RotoGrinders", "SportsLine", "SwishAnalytics")
  
  # download and save projections from all data sources
  if (sourceFlag == 1) {
    for (i in 1:length(sources)) {
      source(paste0("Scripts/", sources[i], ".R"))
    }
  }
  # load data frames
  for (i in 1:length(sources)) {
    load(paste0("Data/", sFlag, "/", sources[i], ".RData"))
  }
  
  # join projection sources
  pool <- full_join(fantasypros, numberFire, by = "Player") %>%
    full_join(rotogrinders, by = "Player") %>%
    # full_join(rotowire, by = "Player") %>%
    full_join(sportsline, by = "Player") %>%
    full_join(swishanalytics, by = "Player") %>%
    full_join(b2b, by = "Team")
  # if (sFlag == "FD") {
  #   pool <- full_join(pool, sportingcharts, by = "Player")
  # }
  # keep only those players who have projections from all sources
  # pool <- na.omit(pool)
  
  # enable secondary position column for DraftKings
  if (sFlag == "FD") {
    posStr <- "Pos"
  } else {
    posStr <- c("Pos", "Pos2")
  }
  
  # rearrange columns
  pool <- cbind(pool[, c("Player", "Team", posStr, "Opp", "Tipoff", "Salary",
                         sources)],
                pool[, -which(colnames(pool) %in% c("Player", "Team", posStr,
                                                    "Opp", "Tipoff", "Salary",
                                                    sources))])
  
  # mean projected points
  poolMean <- sapply(data.frame(t(pool[, sources])), mean, na.rm = T)
  # standard error of projected points
  poolSE <- sapply(data.frame(t(pool[, sources])), calcSE)
  # store Mean and SE in player pool
  pool$Projection <- round(poolMean, 2)
  pool$SE <- round(poolSE, 2)
  # round some columns to 2 decimal points
  pool[, c("Deviation", "Floor", "Ceil")] <- sapply(pool[, c("Deviation", "Floor", "Ceil")],
                                                    round, digits = 2)
  # rearrange columns
  pool <- cbind(pool[, c("Player", "Team", posStr, "Opp", "Tipoff", "Salary",
                         "Projection", "SE")],
                pool[, -which(colnames(pool) %in% c("Player", "Team", posStr,
                                                    "Opp", "Tipoff", "Salary",
                                                    "Projection", "SE"))])
  # filter out players with very low projections
  pool <- filter(pool, Projection > minPts)
  # sort pool by player mean
  pool <- arrange(pool, desc(Projection))
  
  # remove players who appear on the injury list
  pool <- filter(pool, !(Player %in% injuries$Player))
  # remove players from the exclude list
  pool <- filter(pool, !(Player %in% exclude))
  # remove players from early games
  pool <- filter(pool, !(Tipoff %in% early))
  
  # save player pool
  write.csv(pool, file = paste0("Data/", sFlag, "/projections.csv"), row.names = F)
  write.csv(pool, file = paste0("~/Dropbox/DFS/NBA/", sFlag, "/projections.csv"), row.names = F)

  # scatterplot for all positions
  sPlot <- ggplot(pool, aes(x = Salary, y = Projection, label = Label)) +
    geom_jitter() +
    geom_smooth(method = "lm") +
    geom_text(size = 2, vjust = -0.75, fontface = "bold") +
    ggtitle(paste(site, as.character(Sys.Date()), "All Positions", sep = " :: "),
            subtitle = "@WeTalkDFS_NBA") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    # ggtitle(paste("@wetlkfntsysprts",
    #               paste(site, as.character(Sys.Date()), "All Positions", sep = " :: "),
    #               sep = "\n"))
  print(sPlot)
  ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/", "projAll.jpg"),
         width = 5, height = 5)
  dev.off()

  # scatterplots for each position
  plots <- list()
  pos <- c("PG", "SG", "SF", "PF", "C")
  # set position 2 as blank for FanDuel
  if (sFlag == "FD") {
    pool$Pos2 <- ""
  }
  for (i in 1:length(pos)) {
    plots[[i]] <- ggplot(pool %>% filter(Pos == pos[i] | Pos2 == pos[i]),
                         aes(x = Salary, y = Projection, label = Label)) +
      geom_jitter() +
      geom_smooth(method = "lm") +
      geom_text(size = 2, vjust = -0.75, fontface = "bold") +
      ggtitle(paste(site, as.character(Sys.Date()), pos[i], sep = " :: "),
              subtitle = "@WeTalkDFS_NBA") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    print(plots[[i]])
    ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/proj", pos[i],
                  ".jpg"), width = 5, height = 5)
    dev.off()
  }
}