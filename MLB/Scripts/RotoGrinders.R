getData <- function(position) {
  # download projections from RotoGrinders
  url <- getURL(paste0("https://rotogrinders.com/projected-stats/mlb-", position, "?site=", tolower(site)))
  json <- str_extract(url, 'data = .*')
  json <- sub("^data = ", "", json)
  json <- sub(";$", "", json)
  jsonList <- fromJSON(json)
  
  # projected percentage owned
  # pOwn <- as.numeric(sapply(jsonList, function(x) sub("%", "", x[["pown%"]])))
  
  # convert JSON to data frame
  pool <- data.frame(as.character(sapply(jsonList, function(x) x$player_name), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$position), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$player$hand), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$team), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$opp), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x[["home?"]]), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$dvp), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$dvprank), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$order), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$line), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$total), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x[["o/u"]]), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$salary), stringsAsFactors = F),
                     as.character(sapply(jsonList, function(x) x$points), stringsAsFactors = F),
                     stringsAsFactors = F)
  
  # set column names
  names(pool) <- c("Player", "Pos", "Hand", "Team", "Opp", "Home", "DvP", "DvPRank", "Order", "Line", "Total",
                   "OU", "Salary", "RotoGrinders")
  
  # change SP to P
  pool$Pos[pool$Pos == "SP"] <- "P"
  
  # combine Opp and Home columns
  pool$Opp[pool$Home == F] <- paste0("@", pool$Opp[pool$Home == F])
  pool$Home <- NULL
  
  if (sFlag == "DK") {
    # split position column to account for players with more than one position
    dualPos <- grepl("/", pool$Pos)
    pool$Pos2[!dualPos] <- ""
    pool$Pos2[dualPos] <- sub("/", "", str_extract(pool$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
    pool$Pos[dualPos] <- sub("/", "", str_extract(pool$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
  }
  
  # set numeric columns
  pool[, c("DvP", "DvPRank", "Order", "Line", "Total", "OU", "Salary", "RotoGrinders")] <- 
    sapply(pool[, c("DvP", "DvPRank", "Order", "Line", "Total", "OU", "Salary", "RotoGrinders")], as.numeric)
  
  # calculate spread
  pool$Spread <- (pool$OU - pool$Total) - pool$Total
  pool$Total <- NULL
  
  # reorder columns
  pool <- pool[, c("Player", posStr, "Hand", "Team", "Opp", "DvP", "DvPRank", "Order", "Line", "Spread",
                   "OU", "Salary", "RotoGrinders")]
}

hitter <- getData("hitter")
pitcher <- getData("pitcher")
pool <- rbind(hitter, pitcher)

# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# sort player pool by projected points
pool <- arrange(pool, desc(RotoGrinders))

# save file
rotogrinders <- pool
save(rotogrinders, file = paste0("Data/", sFlag, "/RotoGrinders.RData"), row.names = F)