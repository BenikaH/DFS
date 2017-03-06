# create remote driver for dailyfantasynerd website
startServer()
remDr <- remoteDriver$new()
remDr$open()

# navigate to login page
remDr$navigate("https://dailyfantasynerd.com/login")
# type username and password and submit form
username <- remDr$findElement("id", "input-username")
username$sendKeysToElement(list("aitken711"))
password <- remDr$findElement("id", "input-password")
password$sendKeysToElement(list("scroll$7"))
submit <- remDr$findElement("xpath", '//*[@id="login-form"]/fieldset/div[3]/button')
submit$clickElement()
Sys.sleep(10)

# navigate to dailyfantasynerd projections page
remDr$navigate("https://dailyfantasynerd.com/projections/fanduel/nba")
# download dailyfantasynerd projections
Sys.sleep(10)
pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[2]]

# close remote driver
remDr$close()

# select relevant columns
pool <- pool[, c(1, 6)]
pool[, 2] <- as.numeric(pool[, 2])
# rename columns
names(pool) <- c("Player", "dfn")
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# filter out players whose projection is less than 1
pool <- filter(pool, dfn > 1)

# save file
dfn <- pool
save(dfn, file = "~/OneDrive/R/NBA/Data/dfn.RData")
write.csv(dfn, file = "~/OneDrive/R/NBA/Data/dfn.csv", row.names = F)