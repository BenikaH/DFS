# create remote driver
# if (sFlag == "FD") {
#   cprof <- list(chromeOptions =
#                   list(extensions =
#                          list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
#                   ))
#   rD <- rsDriver(verbose = F, extraCapabilities = cprof)
#   remDr <- rD$client
# }
# navigate to Daily Fantasy Cafe homepage
remDr$navigate("https://www.dailyfantasycafe.com/tools/projections/mlb")
Sys.sleep(1)
# click login link
login <- remDr$findElement("link text", "Login")
login$clickElement()
Sys.sleep(1)
# enter user name
user <- remDr$findElement("name", "username")
user$sendKeysToElement(list("wetlkfntsysprts"))
# enter password
pass <- remDr$findElement("name", "password")
pass$sendKeysToElement(list("wetalk247", key = "enter"))
# navigate to Daily Fantasy Cafe page for daily MLB projections
remDr$navigate("https://www.dailyfantasycafe.com/tools/projections/mlb")
Sys.sleep(1)
# select site
if (sFlag == "FD") {
  siteSelected <- remDr$findElement('xpath', '//*[@id="site-selector"]/div/select/option[1]')
} else if (sFlag == "DK") {
  siteSelected <- remDr$findElement('xpath', '//*[@id="site-selector"]/div/select/option[2]')
}
siteSelected$clickElement()
Sys.sleep(1)
# download projections
tables <- readHTMLTable(htmlParse(remDr$getPageSource()[[1]], encoding = "UTF-8"), stringsAsFactors = F)

# create pitchers table
pitchers <- tables[[1]]
pitchers$Pos <- "P"
pitchers <- pitchers[, c("Name", "Pos", "Team", "Projection", "ParkFactor")]
# create hitters table
hitters <- tables[[2]]
hitters <- hitters[, c("Name", "Pos", "Team", "Projection", "ParkFactor")]
# combine pitchers and hitters
pool <- rbind(pitchers, hitters)
# rename columns
names(pool)[c(1, 4)] <- c("Player", "DFC")
# standardize player names
pool$Player <- sub(" \\(.*\\)$", "", pool$Player)
pool$Player <- sapply(pool$Player, replaceName)
pool$Player <- gsub("(\\'|~)", "", iconv(pool$Player, to = "ASCII//TRANSLIT"))
# convert columns to numeric
pool[, -(1:3)] <- sapply(pool[, -(1:3)], as.numeric)
# retain relevant columns
pool <- pool[, c("Player", "DFC", "ParkFactor")]

# close remote driver
# remDr$close()
# rD$server$stop()

# save file
dfc <- pool
save(dfc, file = paste0("Data/", sFlag, "/DFC.RData"))