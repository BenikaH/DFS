# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(dplyr)
library(stringr)
library(ggplot2)

# set working directory
setwd("~/R/DFS")

# scoring flag
sFlag <- "FD"
# sFlag <- "DK"

# week number
week <- 6

# load functions and league settings
source("./Scripts/functions2.R")
source("./Scripts/leagueSettings.R")
source("./Scripts/actual.R")

# load weekly stats
load(paste0("./Data/Week ", week, "/", sFlag, "/actual.Rdata"))
actual$Player <- sapply(actual$Player, replaceName)
# load projections
load(paste0("./Data/Week ", week, "/", sFlag, "/playerPool.Rdata"))

# join auction values and projections data frames
pool <- left_join(pool, actual, by = c("Player", "Pos"))
# omit rows with NA
# pool <- na.omit(pool)

pool <- pool %>% arrange(-Actual) %>% group_by(Pos) %>% mutate(Rank = row_number())

# # league settings
# nQB <- 1
# nRB <- 2
# nWR <- 3
# nTE <- 1
# nK <- 1
# nDEF <- 1
# budget <- 60000
# # remove drafted players from pool
# pool <- pool[!(pool$Player %in% drafted$Player), ]
# # calculate optimal team
# optimalTeam <- optimizeTeam(pool, nQB, nRB, nWR, nTE, nK, nDEF, budget)

# create column for player last names
pool$Last[pool$Pos != "DEF"] <- sub("^\\S+ ", "", pool$Player[pool$Pos != "DEF"])
pool$Last[pool$Pos == "DST"] <- str_extract(pool$Player[pool$Pos == "DST"],
                                            " \\w+$")

# scatterplots for each position
plots <- list()
if (sFlag == "FD") {
  pos <- c("QB", "RB", "WR", "TE", "K", "DST")
} else if (sFlag == "DK") {
  pos <- c("QB", "RB", "WR", "TE", "DST")
}
for (i in 1:length(pos)) {
  plots[[i]] <- ggplot(pool %>% filter(Pos == pos[i]),
                       aes(x = Projection, y = Actual, label = Last)) +
    geom_jitter() +
    geom_text(size = 2, vjust = -0.75) +
    geom_smooth(method = "lm") +
    ggtitle(pos[i])
  print(plots[[i]])
  ggsave(paste0("C:/Users/Matt/Dropbox/DFS/Week ", week, "/", sFlag, "/actual_",
                pos[i], ".jpg"), width = 5, height = 5)
  dev.off()
}