# create remote driver
# cprof <- list(chromeOptions =
#                 list(extensions =
#                        list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
#                 ))
# rD <- rsDriver(verbose = F, extraCapabilities = cprof)
# remDr <- rD$client

# navigate to Swich Analytics page for daily NBA projections
remDr$navigate("https://swishanalytics.com/optimus/mlb/optimus-x")
Sys.sleep(5)
# click FanDuel button
if (sFlag == "FD") {
  fd <- remDr$findElement('xpath', '/html/body/div[6]/div/div[4]/div[2]/div/div/b/b/a/button/b')
  fd$clickElement()
}
# get player names
names <- remDr$findElements('xpath', '//*[@id="vert-mid"]/div')
names <- as.character(sapply(names, function(x) x$getElementText()))
names <- names[names != ""]
# get projections
proj <- remDr$findElements('xpath', '//*[@id="vert-mid"]/span')
proj <- as.numeric(sapply(proj, function(x) x$getElementText()))
proj <- proj[seq(1, length(proj), 3)]
# create player pool
pool <- data.frame("Player" = names, "SwishAnalytics" = proj, stringsAsFactors = F)

# close remote driver
if (sFlag == "DK") {
  remDr$close()
  rD$server$stop()
}

# standardize player names
pool$Player <- sapply(pool$Player, replaceName)

# save file
swishanalytics <- pool
save(swishanalytics, file = paste0("Data/", sFlag, "/SwishAnalytics.RData"), row.names = F)