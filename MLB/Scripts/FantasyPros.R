# download projections from FantasyPros
pool <- readHTMLTable(getURL(paste0("https://www.fantasypros.com/mlb/", tolower(site),
                                    "-lineup-optimizer.php")), stringsAsFactors = F)[["player-pool"]]

# retain relevant columns
pool <- pool[, c("Player Name", "Pos. Rank", "Proj. Pts")]
# set column names
names(pool) <- c("Player", "Pos", "FantasyPros")
# extract position
pool$Pos <- str_extract(pool$Pos, "^[0-9A-Z]{1,2}")
# extract team name
pool$Player <- sub("(DTD|MiLB|RST|DL10|DL60|NRI|DFA|FA)", "", pool$Player)
pool$Team <- str_extract(pool$Player, "[A-Z]{2,3}$")
# extract player name
pool$Player <- sub(", [A-Z]{2,3}$", "", pool$Player)
pool$Player <- sapply(pool$Player, replaceName)
# coerce salary as numeric
# pool$Salary <- as.numeric(sub(",", "", sub("\\$", "", pool$Salary)))
# coerce projections as numeric
pool$FantasyPros <- as.numeric(pool$FantasyPros)
# reorder columns
# pool <- pool[, c("Player", "Pos", "Team", "FantasyPros")]
pool <- pool[, c("Player", "FantasyPros")]

# save file
fantasypros <- pool
save(fantasypros, file = paste0("Data/", sFlag, "/FantasyPros.RData"), row.names = F)