# create remote driver
rD <- rsDriver(verbose = F)
remDr <- rD$client

# navigate to Swich Analytics page for daily NBA projections
remDr$navigate("https://swishanalytics.com/optimus/nba/daily-fantasy-projections")
# click FanDuel button
if (sFlag == "FD") {
  fd <- remDr$findElement("xpath", "/html/body/div[3]/div[2]/div[2]/div[1]/div[1]/div/div/button[2]")
  fd$clickElement()
}
# download projections
pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[2]]

# close remote driver
remDr$close()
rD$server$stop()

# extract player position
pool$Pos <- sub("\\)$", "", str_extract(pool$Name, "[A-Z]{1,2}\\)$"))
# extract player name
pool$Name <- sub(" \\(\\S+\\)$", "", pool$Name)
# retain relevant columns
pool <- pool[, c("Name", "Proj Pts")]
# set column names
names(pool) <- c("Player", "SwishAnalytics")
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# coerce projections as numeric
pool$SwishAnalytics <- as.numeric(pool$SwishAnalytics)

# save file
swishanalytics <- pool
save(swishanalytics, file = paste0("Data/", sFlag, "/SwishAnalytics.RData"), row.names = F)