if (sFlag == "FD") {
  # download projections from numberFire
  batters <- readHTMLTable(getURL("https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections"),
                           stringsAsFactors = F)[[4]][, 1:2]
  pitchers <- readHTMLTable(getURL("https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections/pitchers"),
                           stringsAsFactors = F)[[4]][, 1:2]
} else if (sFlag == "DK") {
  # create remote driver
  # cprof <- list(chromeOptions =
  #                 list(extensions =
  #                        list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
  #                 ))
  # rD <- rsDriver(verbose = F, extraCapabilities = cprof)
  # remDr <- rD$client
  # navigate to numberFire page for daily NBA projections
  remDr$navigate("https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections")
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
  # download batter projections
  batters <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[4]][, 1:2]
  Sys.sleep(5)
  # click login link
  login <- remDr$findElement("link text", "Click here to log in!")
  login$clickElement()
  Sys.sleep(1)
  # click "x" to dismiss pop-up
  x <- remDr$findElement('xpath', '/html/body/div[4]/span')
  x$clickElement()
  Sys.sleep(1)
  # click dropdown menu for positions
  posSelect <- remDr$findElement('xpath', '/html/body/main/div[2]/div[2]/section/div[3]/span[1]')
  posSelect$clickElement()
  Sys.sleep(1)
  # select pitchers
  pitchersSelect <- remDr$findElement('xpath', '/html/body/main/div[2]/div[2]/section/div[3]/select[1]/option[2]')
  pitchersSelect$clickElement()
  Sys.sleep(1)
  # download pitcher projections
  pitchers <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[4]][, 1:2]
  # close remote driver
  # remDr$close()
  # rD$server$stop()
}

# rename columns
names(batters) <- c("Player", "numberFire")
names(pitchers) <- c("Player", "numberFire")
# combine batters and pitchers
pool <- rbind(batters, pitchers)
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