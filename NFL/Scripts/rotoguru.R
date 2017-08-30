# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(stringr)
library(dplyr)

# set week number
week <- 1
# set game (FanDuel = "fd" and DraftKings = "dk")
game <- "dk"
# set column names
header <- paste0("Week;Year;GID;Name;Pos;Team;h/a;Oppt;", toupper(game), " points;", toupper(game), " salary")

# read html file
scsv <- readLines(paste0("http://rotoguru1.com/cgi-bin/fyday.pl?week=", week, "&game=", game, "&scsv=1"))

# get first line of table
firstLine <- grep(header, scsv) + 1
# get last line of table
lastLine <- grep("</pre>", scsv) - 1
# extract semicolon separated file from html
scsv <- scsv[firstLine:lastLine]

# convert semicolon separated file to data frame
df <- data.frame(do.call(rbind, strsplit(scsv, ";", fixed = T)))
# set column names
names(df) <- strsplit(header, ";", fixed = T)[[1]]