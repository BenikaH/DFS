# create remote driver
rD <- rsDriver(verbose = F)
remDr <- rD$client

# navigate to numberFire page for daily NBA projections
remDr$navigate(paste0("http://www.dailyfantasyfuel.com/nba/projections/", tolower(site)))
Sys.sleep(1)

# scrape projections
webElems <- remDr$findElements('xpath', paste0('//*[@id="projections"]/div[1]/div/div[5]/div'))
getAttributes <- function(webElem) {
  as.character(c(paste(webElem$getElementAttribute("data-first_name"),
                       webElem$getElementAttribute("data-last_name")),
                 webElem$getElementAttribute("data-ppg_projected")))
}

# create player pool
pool <- data.frame(t(sapply(webElems, getAttributes)), stringsAsFactors = F)
# set column names
names(pool) <- c("Player", "FantasyFuel")
# convert projections from character to numeric
pool$FantasyFuel <- as.numeric(pool$FantasyFuel)

# close remote driver
remDr$close()
rD$server$stop()

# save file
fantasyfuel <- pool
save(fantasyfuel, file = paste0("Data/", sFlag, "/FantasyFuel.RData"))