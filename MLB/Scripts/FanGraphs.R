# positions
pos <- c("P", "C", "1B", "2B", "SS", "3B", "RF", "CF", "LF", "DH")

# create remote driver
cprof <- list(chromeOptions =
                list(extensions =
                       list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
                ))
rD <- rsDriver(verbose = F, extraCapabilities = cprof)
remDr <- rD$client
for (i in 1:length(pos)) {
  # navigate to FanGraphs page for daily MLB projections
  if (pos[i] == "P") {
    remDr$navigate("http://www.fangraphs.com/dailyprojections.aspx?pos=all&stats=pit&type=sabersim&team=0&lg=all&players=0")
  } else {
    remDr$navigate(paste0("http://www.fangraphs.com/dailyprojections.aspx?pos=",
                          tolower(pos[i]), "&stats=bat&type=sabersim&team=0&lg=all&players=0"))
  }
  Sys.sleep(1)
  # get projections
  newPool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)
  newPool <- newPool[[length(newPool)]][, c("Name", "FanDuel", "DraftKings")]
  # add position column
  newPool$Pos <- pos[i]
  # update player pool
  if (i == 1) {
    pool <- newPool
  } else {
    pool <- rbind(pool, newPool)
  }
}

# close remote driver
# remDr$close()
# rD$server$stop()

# rearrange columns
pool <- pool[, c("Name", "Pos", "FanDuel", "DraftKings")]
# rename columns
names(pool)[1] <- "Player"
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# convert projections to numeric
pool[, c("FanDuel", "DraftKings")] <- sapply(pool[, c("FanDuel", "DraftKings")], as.numeric)

# save FD file
fangraphs <- pool[, c("Player", "FanDuel")]
names(fangraphs)[2] <- "FanGraphs"
save(fangraphs, file = "Data/FD/FanGraphs.RData")
# save DK file
fangraphs <- pool[, c("Player", "DraftKings")]
names(fangraphs)[2] <- "FanGraphs"
save(fangraphs, file = "Data/DK/FanGraphs.RData")