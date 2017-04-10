# create remote driver
# cprof <- list(chromeOptions =
#                 list(extensions =
#                        list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
#                 ))
# rD <- rsDriver(verbose = F, extraCapabilities = cprof)
# remDr <- rD$client

# navigate to Lineup Lab page for daily MLB projections
if (sFlag == "FD") {
  remDr$navigate("https://www.lineuplab.com/MLB/fanduel")
} else if (sFlag == "DK") {
  remDr$navigate("https://www.lineuplab.com/MLB/DK")
}
Sys.sleep(1)

# click projections button
projButton <- remDr$findElement("id", "load-projection-source-button")
projButton$clickElement()
Sys.sleep(3)
# get players
players <- remDr$findElements("class name", "pl-player-name-col")
players <- as.character(sapply(players, function(x) x$getElementText()))[-1]
# get salaries
salary <- remDr$findElements("class name", "player-salary")
salary <- as.character(sapply(salary, function(x) x$getElementText()))[-1]
salary <- as.numeric(sub("\\$", "", salary))
# get projections
proj <- remDr$findElements('class name', 'player-points')
proj <- as.numeric(sapply(proj, function(x) x$getElementText()))[seq(4, length(proj), 2)]
# cull vectors
minLength <- min(c(length(players), length(salary), length(proj)))
players <- players[1:minLength]
salary <- salary[1:minLength]
proj <- proj[1:minLength]
# create player pool
pitchers <- data.frame("Player" = players, "Salary" = salary, "lineupLab" = proj, stringsAsFactors = F)
pitchers$lineupLab <- round(pitchers$lineupLab * pitchers$Salary / 1000, 2)

# click dropdown
dropdown <- remDr$findElement('xpath', '//*[@id="main_container"]/div[4]/div/div[3]/div[4]/div[1]/ul/li[2]/div/a')
dropdown$clickElement()
# click hitters link
hittersLink <- remDr$findElement("link text", "Hitters")
hittersLink$clickElement()
Sys.sleep(1)

# get player players
players <- remDr$findElements("class name", "pl-player-name-col")
players <- as.character(sapply(players, function(x) x$getElementText()))[-1]
# get salaries
salary <- remDr$findElements("class name", "player-salary")
salary <- as.character(sapply(salary, function(x) x$getElementText()))[-1]
salary <- as.numeric(sub("\\$", "", salary))
# get projections
proj <- remDr$findElements('class name', 'player-points')
proj <- as.numeric(sapply(proj, function(x) x$getElementText()))[seq(4, length(proj), 2)]
# cull vectors
minLength <- min(c(length(players), length(salary), length(proj)))
players <- players[1:minLength]
salary <- salary[1:minLength]
proj <- proj[1:minLength]
# create player pool
hitters <- data.frame("Player" = players, "Salary" = salary, "lineupLab" = proj, stringsAsFactors = F)
hitters$lineupLab <- round(hitters$lineupLab * hitters$Salary / 1000, 2)

# close remote driver
# remDr$close()
# rD$server$stop()

# combine pitchers and hitters
pool <- rbind(pitchers, hitters)
# standardize player players
pool$Player <- sapply(pool$Player, replaceName)
# remove salary column
pool$Salary <- NULL
# remove duplicated rows
pool <- pool[!duplicated(pool), ]

# save file
lineuplab <- pool
save(lineuplab, file = paste0("Data/", sFlag, "/lineupLab.RData"), row.players = F)