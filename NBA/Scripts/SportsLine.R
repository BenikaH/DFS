# positions
pos <- c("point-guard", "shooting-guard", "small-forward", "power-forward",
         "center")
for (i in 1:5) {
  # get URL
  url <- getURL(paste0("http://www.sportsline.com/nba/player-projections/player-stats/",
                pos[i], "/"))
  # parsed HTML
  parsedHTML <- htmlParse(url)
  # player table
  string <- xmlValue(getNodeSet(parsedHTML, '//*[@id="App"]/div[4]/section/div/div[1]/div')[[1]])
  # clean up player table
  string <- sub("^ \n        \n        \n         \n          \n           \n           \n             \n            \n             ",
                "", string)
  string <- sub("  \n            \n            \n            \n          \n         \n        \n      $",
                "", string)
  strSplit <- str_split(string, "  \n            \n            \n            \n          \n         \n        \n        \n         \n          \n           \n            \n            \n             ")[[1]]
  rows <- str_split(strSplit, "  \n            \n            \n            \n          \n           \n            \n            \n             ")
  rows <- as.data.frame(t(as.data.frame(rows)), stringsAsFactors = F)
  rownames(rows) <- NULL
  rowsNames <- str_split(rows[1, 1], "\n             \n             \n            \n            \n          \n           \n           \n             \n            \n             ")[[1]]
  names(rows)[1:15] <- rowsNames[1:15]
  lastStr <- str_split(rowsNames[16], "\n             \n             \n            \n            \n         \n        \n        \n         \n          \n           \n            \n            \n             ")[[1]]
  names(rows)[16] <- lastStr[1]
  rows[1, 1] <- lastStr[2]
  # save player table
  if (i == 1) {
    pool <- rows
  } else {
    pool <- rbind(pool, rows)
  }
}
# rename some columns
names(pool)[1] <- "Player"
names(pool)[5] <- "TRB"

# scoring system
if (sFlag == "FD") {
  scoring <- data.frame(t(c(1, 1.2, 1.5, 0, 2, 2, -1)))
} else if (sFlag == "DK") {
  scoring <- data.frame(t(c(1, 1.25, 1.5, 0.5, 2, 2, -0.5)))
}
names(scoring) <- c("PTS", "TRB", "AST", "3PM", "BLK", "STL", "TO")

# select relevant columns
pool <- pool[, c("Player", names(scoring))]
# set numeric columns
pool[, -1] <- sapply(pool[, -1], as.numeric)
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# calculate fantasy points
pool$SportsLine <- as.numeric(as.matrix(pool[, -1]) %*% as.numeric(scoring[names(pool)[-1]]))
if (sFlag == "DK") {
  for (i in 1:nrow(pool)) {
    pool$SportsLine[i] <- pool$SportsLine[i] + bonus(pool[i, ])
  }
}
# retain relevant columns
pool <- pool[, c("Player", "SportsLine")]

# save file
sportsline <- pool
save(sportsline, file = paste0("Data/", sFlag, "/SportsLine.RData"), row.names = F)