# download projections from FantasyPros
url <- getURL(paste0("https://www.fantasypros.com/nba/", tolower(site),
                     "-lineup-optimizer.php"))
pool <- readHTMLTable(url, stringsAsFactors = F)[["player-pool"]]

# retain relevant columns
pool <- pool[, 1:6]
# set column names
names(pool) <- c("Player", "Pos", "Opp", "Tipoff", "FantasyPros", "Salary")
# extract position
pool$Pos <- str_extract(pool$Pos, "^[A-Z]{1,2}")
# extract team name
pool$Player <- sub("DTD", "", pool$Player)
pool$Team <- str_extract(pool$Player, "[A-Z]{3}$")
# extract player name
pool$Player <- sub(", [A-Z]{3}$", "", pool$Player)
pool$Player <- sapply(pool$Player, replaceName)
# coerce salary as numeric
pool$Salary <- as.numeric(sub(",", "", sub("\\$", "", pool$Salary)))
# coerce projections as numeric
pool$FantasyPros <- as.numeric(pool$FantasyPros)
# reorder columns
pool <- pool[, c("Player", "Team", "Pos", "Opp", "Tipoff", "Salary", "FantasyPros")]

# get position from Rotogrinders instead
pool$Pos <- NULL

# save file
fantasypros <- pool
save(fantasypros, file = paste0("Data/", sFlag, "/FantasyPros.RData"), row.names = F)