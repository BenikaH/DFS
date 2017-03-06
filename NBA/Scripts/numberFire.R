if (sFlag == "FD") {
  # download projections from numberFire
  pool <- readHTMLTable("http://www.numberfire.com/nba/daily-fantasy/daily-basketball-projections",
                        stringsAsFactors = F)[[4]]
} else if (sFlag == "DK") {
  # create remote driver
  rD <- rsDriver(verbose = F)
  remDr <- rD$client
  # navigate to numberFire page for daily NBA projections
  remDr$navigate("http://www.numberfire.com/nba/daily-fantasy/daily-basketball-projections")
  Sys.sleep(5)
  # click login link
  login <- remDr$findElement("link text", "Click here to log in!")
  login$clickElement()
  Sys.sleep(1)
  # click "x" to dismiss pop-up
  x <- remDr$findElement('xpath', '/html/body/div[4]/span')
  x$clickElement()
  Sys.sleep(1)
  # click dropdown menu for platform
  dropdown <- remDr$findElement('xpath', '/html/body/main/div[2]/div[2]/div/div[2]/div[1]/div')
  dropdown$clickElement()
  Sys.sleep(1)
  # select DraftKings
  dk <- remDr$findElement('xpath', '/html/body/main/div[2]/div[2]/div/div[2]/div[1]/div/ul/li[2]')
  dk$clickElement()
  Sys.sleep(1)
  # download projections
  pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[4]]
  # close remote driver
  remDr$close()
  rD$server$stop()
}

# remove newline characters and spaces from column names
names(pool) <- gsub("\n|\\s", "", names(pool))
# retain relevant columns
pool <- pool[, c("Player", "FP")]
# set column names
names(pool) <- c("Player", "numberFire")
# extract first initial and last name
pool$Label <- str_extract(pool$Player, "^[A-Z]\\. \\S+")
# extract player name
pool$Player <- str_extract(pool$Player, "\n\\s+[A-Z].*\n")
pool$Player <- sub("\\s+$", "", sub("^\\s+", "", pool$Player))
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# coerce projections as numeric
pool$numberFire <- as.numeric(pool$numberFire)

# save file
numberFire <- pool
save(numberFire, file = paste0("Data/", sFlag, "/numberFire.RData"))

# # numberFire URL
# html <- getURL("http://www.numberfire.com/nba/fantasy/fantasy-basketball-projections")
# 
# # extract player name
# playerString <- ",\\\"name\\\":\\\"([A-Z]|[a-z]|\\.|\\s|-|\\')+"
# pool <- str_extract_all(html, playerString)[[1]][-(1:30)]
# pool <- data.frame(sub(",\\\"name\\\":\\\"", "", pool))
# names(pool) <- "Player"
# pool$Player <- as.character(pool$Player)
# pool$Player <- sapply(pool$Player, replaceName)
# 
# # extract team IDs
# idString <- "\\\"id\\\":\\\"[0-9]{1,2}"
# id <- str_extract_all(html, idString)[[1]][4:33]
# id <- sub("\\\"id\\\":\\\"", "", id)
# 
# # extract team abbreviations
# abbrevString <- ",\\\"abbrev\\\":\\\"[A-Z]{2,3}"
# abbrev <- str_extract_all(html, abbrevString)[[1]]
# abbrev <- sub(",\\\"abbrev\\\":\\\"", "", abbrev)
# abbrev[10] <- "GSW"
# abbrev[19] <- "NOR"
# abbrev[20] <- "NYK"
# abbrev[24] <- "PHO"
# abbrev[27] <- "SAS"
# abbrev[29] <- "UTH"
# abbrev[30] <- "WAS"
# 
# # extract team name
# teamString <- ",\\\"team_id\\\":\\\"[0-9]{1,2}"
# team <- str_extract_all(html, teamString)[[1]]
# team <- sub(",\\\"team_id\\\":\\\"", "", team)
# for (i in 1:length(team)) {
#   team[i] <- abbrev[which(id == team[i])]
# }
# pool$Team <- team
# 
# # extract projection
# fpString <- "fanduel_fp\":.{1,5},"
# fp <- str_extract_all(html, fpString)[[1]]
# fp <- sub("fanduel_fp\":", "", fp)
# fp <- sub(",", "", fp)
# pool$numberFire <- as.numeric(fp)
# # filter out players whose projection is less than 1
# pool <- filter(pool, numberFire > 1)