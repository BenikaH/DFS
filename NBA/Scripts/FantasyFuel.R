# create remote driver
rD <- rsDriver(verbose = F)
remDr <- rD$client

# navigate to numberFire page for daily NBA projections
remDr$navigate(paste0("http://www.dailyfantasyfuel.com/nba/projections/", tolower(site)))
Sys.sleep(1)

# initialize data frame
pool <- data.frame("Player" = character(), "FantasyFuel" = character(), stringsAsFactors = F)
# scrape projections
for (i in 1:1000) {
  webElem <- tryCatch({
    remDr$findElement('xpath', paste0('//*[@id="projections"]/div[1]/div/div[5]/div[', i, ']'))
  }, error = function(e) {
    NULL
  })
  if (is.null(webElem)) {
    break
  }
  pool[i, ] <- c(paste(webElem$getElementAttribute("data-first_name"),
                       webElem$getElementAttribute("data-last_name")),
                 webElem$getElementAttribute("data-ppg_projected"))
}
# convert projections from character to numeric
pool$FantasyFuel <- as.numeric(pool$FantasyFuel)
# close remote driver
remDr$close()
rD$server$stop()

# save file
fantasyfuel <- pool
save(fantasyfuel, file = paste0("Data/", sFlag, "/FantasyFuel.RData"))