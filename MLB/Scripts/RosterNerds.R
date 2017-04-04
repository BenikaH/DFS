# create remote driver
cprof <- list(chromeOptions =
                list(extensions =
                       list(base64encode("Scripts/Adblock-Plus_v1.12.4.crx"))
                ))
rD <- rsDriver(verbose = F, extraCapabilities = cprof)
remDr <- rD$client
# navigate to Roster Nerds page for daily MLB projections
if (sFlag == "FD") {
remDr$navigate("http://mlb.rosternerds.com/")
} else {
  remDr$navigate("http://mlb.rosternerds.com/dk/full//")
}
Sys.sleep(5)
# download projections
pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[["players"]]
# close remote driver
remDr$close()
rD$server$stop()

# retain relevant columns
# pool <- pool[, c("Player", "Pos", "Team", "Proj")]
pool <- pool[, c("Player", "Proj")]
# change name of projection column
# names(pool)[4] <- "RosterNerds"
names(pool)[2] <- "RosterNerds"

# # save file
rosternerds <- pool
save(rosternerds, file = paste0("Data/", sFlag, "/RosterNerds.RData"))