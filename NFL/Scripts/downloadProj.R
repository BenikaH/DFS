# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(RSelenium)
library(XML)
library(stringr)
library(dplyr)

source("functions.R")

# set week number
week <- 1

# # create remote driver
# startServer()
# remDr <- remoteDriver(browserName = "chrome") # use Chrome here if Selenium not working with Firefox
# remDr$open()
# 
# # navigate to login page
# remDr$navigate("http://apps.fantasyfootballanalytics.net/projections")
# # click download button
# download <- remDr$findElement("id", "DownloadButton")
# download$clickElement()
# # click "Custom Rankings"
# custom <- remDr$findElement("xpath", "/html/body/div[2]/div/div[1]/div[2]/div/ul/li[1]/a")
# custom$clickElement()
# 
# # wait for file to download
# Sys.sleep(5)
# 
# # move projections file to current directory
# file.rename("C:/Users/Matt/Downloads/FFA-CustomRankings.csv",
#             "C:/Users/Matt/Documents/R/Fantasy Football/FFA-CustomRankings.csv")

# read projections file
proj <- read.csv(paste0("~/OneDrive/R/NFL/Data/Week ", week,
                        "/FFA-CustomRankings.csv"),
                 row.names = NULL, stringsAsFactors = F)

# reset column names
colnames(proj) <- c(colnames(proj)[-1], "dummy")
proj <- proj[, 1:ncol(proj) - 1]
# select relevant columns
proj <- proj[, c("playername", "position", "team", "points", "upper", "lower",
                 "risk")]
colnames(proj) <- c("Player", "Pos", "Team", "Projection", "Upper", "Lower", "Risk")
# convert Risk column to numeric
proj$Risk <- as.numeric(proj$Risk)

proj$Team[proj$Team == "JAC"] <- "JAX"
proj$Pos[proj$Pos == "DST"] <- "DEF"
proj$Player <- sapply(proj$Player, replaceName)

# save values dataframe
save(proj, file = paste0("../Data/Week ", week, "/downloadProj.Rdata"))
write.csv(proj, file = paste0("../Data/Week ", week, "/downloadProj.csv"),
          row.names = F)