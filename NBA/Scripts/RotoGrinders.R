# download projections from RotoGrinders
url <- getURL(paste0("https://rotogrinders.com/projected-stats/nba-player?site=",
                     tolower(site)))
json <- str_extract(url, 'data = .*')
json <- sub("^data = ", "", json)
json <- sub(";$", "", json)
jsonList <- fromJSON(json)

# projected percentage owned
# pOwn <- as.numeric(sapply(jsonList, function(x) sub("%", "", x[["pown%"]])))

# convert JSON to data frame
pool <- data.frame(as.character(sapply(jsonList, function(x) x$player_name), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$position), stringsAsFactors = F),
                   # sapply(jsonList, function(x) x$team),
                   # sapply(jsonList, function(x) x$salary),
                   as.character(sapply(jsonList, function(x) x$points), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$deviation), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$floor), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$ceil), stringsAsFactors = F),
                   # pOwn,
                   as.character(sapply(jsonList, function(x) x[["o/u"]]), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$line), stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$total), stringsAsFactors = F),
                   stringsAsFactors = F)

# set column names
names(pool) <- c("Player", "Pos", "RotoGrinders", "Deviation", "Floor", "Ceil",
                 "OU", "Line", "Total")

if (sFlag == "DK") {
  # split position column to account for players with more than one position
  dualPos <- grepl("/", pool$Pos)
  pool$Pos2[!dualPos] <- ""
  pool$Pos2[dualPos] <- sub("/", "", str_extract(pool$Pos[dualPos], "/[A-Z]{1,2}$"))
  pool$Pos[dualPos] <- sub("/", "", str_extract(pool$Pos[dualPos], "^[A-Z]{1,2}/"))
  # reorder columns
  pool <- pool[, c("Player", "Pos", "Pos2", "RotoGrinders", "Deviation", "Floor", "Ceil",
                   "OU", "Line", "Total")]
}

# set numeric columns
pool[, c("RotoGrinders", "Deviation", "Floor", "Ceil", "OU", "Line", "Total")] <- 
  sapply(pool[, c("RotoGrinders", "Deviation", "Floor", "Ceil", "OU", "Line", "Total")], as.numeric)

# calculate spread
pool$Spread <- (pool$OU - pool$Total) - pool$Total
pool$Total <- NULL

# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# sort player pool by projected points
pool <- arrange(pool, desc(RotoGrinders))

# save file
rotogrinders <- pool
save(rotogrinders, file = paste0("Data/", sFlag, "/RotoGrinders.RData"), row.names = F)