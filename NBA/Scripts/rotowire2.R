# create remote driver for Rotowire website
startServer()
remDr <- remoteDriver$new()
remDr$open()

# navigate to login page
remDr$navigate("http://www.rotowire.com/users/loginuser2.htm")
# type username and password and submit form
username <- remDr$findElement("name", "username")
username$sendKeysToElement(list("aitken711"))
password <- remDr$findElement("name", "p1")
password$sendKeysToElement(list("scroll$7"))
submit <- remDr$findElement("name", "submit")
submit$clickElement()

# navigate to Rotowire projections page
# remDr$navigate("http://www.rotowire.com/daily/nba/value-report.htm")
remDr$navigate("http://www.rotowire.com/basketball/daily_projections.htm")
# download Rotowire projections
pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[1]]

# close remote driver
remDr$close()

# re-format player names
names(pool)[1] <- "Player"
pool$Player <- str_extract(pool$Player, "[A-Z]([a-z]+-[A-Z][a-z]+|[a-z]+|[a-z][A-Z][a-z]+|'[A-Z][a-z]+), [A-Z]([a-z]+-[A-Z][a-z]+|[a-z][A-Z][a-z]{2,}|[a-z]+|\\.[A-Z]\\.|'[A-Z][a-z]+)")
firstName <- sub(".*, ", "", pool$Player)
lastName <- sub(", .*", "", pool$Player)
pool$Player <- paste(firstName, lastName)
pool$Player <- sapply(pool$Player, replaceName)

# change abbreviation for New Orleans and Utah
pool$Team[pool$Team == "NOP"] <- "NOR"
pool$Team[pool$Team == "UTA"] <- "UTH"

# select relevant columns
pool <- pool[, c(1, 3, 7)]
names(pool)[3] <- "Rotowire"
pool$Rotowire <- as.numeric(pool$Rotowire)
# filter out players whose projection is less than 1
pool <- filter(pool, Rotowire > 1)

# # save file
rotowire <- pool
save(rotowire, file = "~/OneDrive/R/NBA/Data/rotowire.RData")
write.csv(rotowire, file = "~/OneDrive/R/NBA/Data/rotowire.csv", row.names = F)