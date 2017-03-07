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
setwd("~/OneDrive/GitHub/DFS/NBA")

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
    scoring <- data.frame(t(c(3, 2, 1, 1.2, 1.5, 2, 2, -1)))
  } else if (sFlag == "DK") {
    scoring <- data.frame(t(c(3.5, 2, 1, 1.25, 1.5, 2, 2, -0.5)))
  }
  names(scoring) <- c("Three", "Two", "FT", "TRB", "AST", "BLK", "STL", "TOV")
  
  # yesterday's date
  dateChar <- strsplit(as.character(Sys.Date() - 1), "-")[[1]]
  year <- dateChar[1]
  month <- dateChar[2]
  day <- dateChar[3]
  
  # download previous day's stats from Basketball Reference
  url <- paste0("http://www.basketball-reference.com/friv/dailyleaders.cgi?month=", month, "&day=", day,
                "&year=", year)
  actual <- readHTMLTable(url, stringsAsFactors = F)[[1]]
  
  # insert column names
  names(actual)[3] <- "Team"
  names(actual)[4] <- "At"
  names(actual)[6] <- "W/L"
  names(actual)[11] <- "Three"
  
  # change abbreviation for Brooklyn
  actual$Team[actual$Team == "BRK"] <- "BKN"
  
  # filter out header rows in the middle of the table
  actual <- filter(actual, Player != "Player")
  # set numeric columns
  actual[, -(1:7)] <- sapply(actual[, -(1:7)], as.numeric)
  # calculate 2-pt FG
  actual$Two <- actual$FG - actual$Three
  # select relevant columns
  actual <- actual[, c("Player", names(scoring))]
  # standardize player names
  actual$Player <- sapply(actual$Player, replaceName)
  # calculate total points
  actual$PTS <- 3*actual$Three + 2*actual$Two + actual$FT
  # rearrange columns
  actual <- cbind(actual[, c("Player", "PTS")],
                  actual[, -which(colnames(actual) %in% c("Player", "PTS"))])
  # calculate fantasy points
  actual$Actual <- as.numeric(as.matrix(actual[, -(1:2)]) %*% as.numeric(scoring[names(actual)[-(1:2)]]))
  if (sFlag == "DK") {
    for (i in 1:nrow(actual)) {
      actual$Actual[i] <- actual$Actual[i] + bonus(actual[i, ])
    }
  }
  
  # read projections file
  pool <- read.csv(paste0("Data/", sFlag, "/projections.csv"), stringsAsFactors = F)
  # join projections with actual results
  pool <- left_join(pool, actual, by = "Player")
  # calculate relative player value
  pool$Value <- pool$Projection / (pool$Salary / 1000)
  pool$actValue <- pool$Actual / (pool$Salary / 1000)
  # remove faulty actual results
  pool <- pool[pool$Actual >=0, ]
  
  # save actual results
  write.csv(pool, file = paste0("Data/", sFlag, "/results.csv"), row.names = F)
  write.csv(pool, file = paste0("~/Dropbox/DFS/NBA/", sFlag, "/results.csv"), row.names = F)
  
  # # correlation between projections and results
  # rho <- cor(pool$Projection, pool$Actual, use = "complete.obs")
  
  # median player value
  mid <- median(pool$Value, na.rm = T)
  
  # set position 2 as blank for FanDuel
  if (sFlag == "FD") {
    pool$Pos2 <- ""
  }
  
  # scatterplot
  scatter <- ggplot(pool, aes(x = Projection, y = Actual, label = Label, color = Value)) +
    geom_jitter() +
    # scale_colour_gradientn(colors = c("blue", "violet", "red"), name = "Proj.\nValue") +
    # scale_color_gradient2(low = "blue", mid = "violet", high = "red", midpoint = mid, name = "Proj.\nValue") +
    # scale_color_distiller(type = "div", palette = "RdBu", name = "Proj.\nValue") +
    scale_color_gradient(low = "blue", high = "red", name = "Proj.\nValue") +
    geom_smooth(method = "lm", color = "black") +
    geom_text(size = 2, vjust = -0.75, fontface = "bold") +
    # geom_text(data = subset(pool, Exposure > 0.1), size = 3) +
    ggtitle(paste(site, as.character(Sys.Date() - 1), "All Positions", sep = " :: "),
            subtitle = "@WeTalkDFS_NBA") +
    # theme_dark() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  print(scatter)
  ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/", "actAll.jpg"),
         width = 5, height = 5)
  dev.off()
  
  # scatterplots for each position
  plots <- list()
  pos <- c("PG", "SG", "SF", "PF", "C")
  for (i in 1:length(pos)) {
    mid <- median(pool$Value[pool$Pos == pos[i] | pool$Pos2 == pos[i]], na.rm = T)
    plots[[i]] <- ggplot(pool %>% filter(Pos == pos[i] | Pos2 == pos[i]),
                         aes(x = Projection, y = Actual, label = Label, color = Value)) +
      # scale_colour_gradientn(colors = c("blue", "violet", "red"), name = "Proj.\nValue") +
      # scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = mid, name = "Proj.\nValue") +
      # scale_color_distiller(type = "div", palette = "RdBu", name = "Proj.\nValue") +
      scale_color_gradient(low = "blue", high = "red", name = "Proj.\nValue") +
      geom_jitter() +
      geom_smooth(method = "lm", color = "black") +
      geom_text(size = 2, vjust = -0.75, fontface = "bold") +
      ggtitle(paste(site, as.character(Sys.Date() - 1), pos[i], sep = " :: "),
              subtitle = "@WeTalkDFS_NBA") +
      # theme_dark() +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    print(plots[[i]])
    ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/act", pos[i],
                  ".jpg"), width = 5, height = 5)
    dev.off()
  }
    
  # scatterplot
  scatter <- ggplot(pool, aes(x = Value, y = actValue, label = Label)) +
    geom_jitter() +
    labs(x = "Projected Value", y = "Actual Value") +
    geom_smooth(method = "lm") +
    geom_text(size = 2, vjust = -0.75, fontface = "bold") +
    # geom_text(data = subset(pool, Exposure > 0.1), size = 3) +
    ggtitle(paste(site, as.character(Sys.Date() - 1), "All Positions", sep = " :: "),
            subtitle = "@WeTalkDFS_NBA") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  print(scatter)
  ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/", "valAll.jpg"),
         width = 5, height = 5)
  dev.off()
  
  # scatterplots for each position
  plots <- list()
  pos <- c("PG", "SG", "SF", "PF", "C")
  for (i in 1:length(pos)) {
    plots[[i]] <- ggplot(pool %>% filter(Pos == pos[i] | Pos2 == pos[i]),
                         aes(x = Value, y = actValue, label = Label)) +
      labs(x = "Projected Value", y = "Actual Value") +
      geom_jitter() +
      geom_smooth(method = "lm") +
      geom_text(size = 2, vjust = -0.75, fontface = "bold") +
      ggtitle(paste(site, as.character(Sys.Date() - 1), pos[i], sep = " :: "),
              subtitle = "@WeTalkDFS_NBA") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    print(plots[[i]])
    ggsave(paste0("~/Dropbox/DFS/NBA/", sFlag, "/val", pos[i],
                  ".jpg"), width = 5, height = 5)
    dev.off()
  }
}