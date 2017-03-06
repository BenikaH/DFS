# download Yahoo DFS player pool
pool <- readHTMLTable("http://www.fantasypros.com/nba/yahoo-lineup-optimizer.php",
                      stringsAsFactors = F)[["player-pool"]]
# retain a subset of columns
pool <- pool[, c(1:6)]
# set column names
names(pool) <- c("Player", "Pos", "Opp", "Tipoff", "Yahoo", "Cost")
# remove rows with unranked players
pool <- pool %>% filter(Pos != "NR")

# team name
teams <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL",
           "MEM", "MIA", "MIL", "MIN", "NOR", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR",
           "UTH", "WAS")
# temp <- sapply(pool$Player, str_extract, pattern = teams)
# pool$Team <- temp[!is.na(temp)]
pool$Team <- sub(", ", "", str_extract(pool$Player, ", [A-Z]{3}"))
# player name
pool$Player <- sub(",.*", "", pool$Player)
pool$Player <- sapply(pool$Player, replaceName)
# position
pool$Pos <- sapply(strsplit(pool$Pos, " "), '[[', 1)
# cost
pool$Cost <- sub("\\$", "", pool$Cost)
pool$Cost <- sub(",", "", pool$Cost)

# rearrange column order
pool <- pool[c("Player", "Team", "Pos", "Opp", "Tipoff", "Cost", "Yahoo")]
# change class of numeric columns
pool[, c("Cost", "Yahoo")] <- sapply(pool[, c("Cost", "Yahoo")], as.numeric)
# filter out players whose projection is less than 1
pool <- filter(pool, Yahoo > 1)

# save file
yahoo <- pool
save(yahoo, file = "~/OneDrive/R/NBA/Data/yahoo.RData")
write.csv(yahoo, file = "~/OneDrive/R/NBA/Data/yahoo.csv", row.names = F)