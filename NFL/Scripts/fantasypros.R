# download FanDuel DFS player pool
if (sFlag == "FD") {
  site <- "fanduel"
} else if (sFlag == "DK") {
  site <- "draftkings"
}
pool <- readHTMLTable(getURL(paste0("https://www.fantasypros.com/nfl/",
                                    site, "-lineup-optimizer.php")),
                      stringsAsFactors = F)[["player-pool"]]
# retain a subset of columns
pool <- pool[, c(1:2, 4, 6:7)]
# set column names
names(pool) <- c("Player", "Pos", "Kickoff", "FantasyPros", "Cost")
# remove rows with unranked players
pool <- pool %>% filter(Pos != "NR")

# team name
teams <- c("DAL", "NE", "BAL", "SEA", "ATL", "GB", "IND", "PHI", "DEN", "NYG", "DET", "NO", "SD", "MIN", "PIT",
           "ARI", "CHI", "BUF", "SF", "MIA", "CIN", "LA", "KC", "NYJ", "CAR", "OAK", "HOU", "TEN", "CLE",
           "TB", "WAS", "JAC")
temp <- sapply(pool$Player, str_extract, pattern = teams)
pool$Team <- temp[!is.na(temp)]
# player name
pool$Player <- sub(",.*", "", pool$Player)
# position
pool$Pos <- sapply(strsplit(pool$Pos, " "), '[[', 1)
# cost
pool$Cost <- sub("\\$", "", pool$Cost)
pool$Cost <- sub(",", "", pool$Cost)

# rearrange column order
pool <- pool[c("Player", "Team", "Pos", "Kickoff", "FantasyPros", "Cost")]
# change class of numeric columns
pool[, c("FantasyPros", "Cost")] <- sapply(pool[, c("FantasyPros", "Cost")], as.numeric)

# download rankings
qb <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/qb.php"),
                    stringsAsFactors = F)[["data"]]
# retain a subset of columns
qb <- qb[, c(1:3, 6:7)]
# set column names
names(qb) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
qb$Pos <- "QB"
# remove rows with NA
qb <- na.omit(qb)
# fix player name
qb$Player <- sub(" [A-Z]{2,3}.*", "", qb$Player)

rb <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/half-point-ppr-rb.php"),
                    stringsAsFactors = F)[["data"]]
# retain a subset of columns
rb <- rb[, c(1:3, 6:7)]
# set column names
names(rb) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
rb$Pos <- "RB"
# remove rows with NA
rb <- na.omit(rb)
# fix player name
rb$Player <- sub(" [A-Z]{2,3}.*", "", rb$Player)

wr <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/half-point-ppr-wr.php"),
                    stringsAsFactors = F)[["data"]]
# retain a subset of columns
wr <- wr[, c(1:3, 6:7)]
# set column names
names(wr) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
wr$Pos <- "WR"
# remove rows with NA
wr <- na.omit(wr)
# fix player name
wr$Player <- sub(" [A-Z]{2,3}.*", "", wr$Player)

te <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/half-point-ppr-te.php"),
                    stringsAsFactors = F)[["data"]]
# retain a subset of columns
te <- te[, c(1:3, 6:7)]
# set column names
names(te) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
te$Pos <- "TE"
# remove rows with NA
te <- na.omit(te)
# fix player name
te$Player <- sub(" [A-Z]{2,3}.*", "", te$Player)

k <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/k.php"),
                   stringsAsFactors = F)[["data"]]
# retain a subset of columns
k <- k[, c(1:3, 6:7)]
# set column names
names(k) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
k$Pos <- "K"
# remove rows with NA
k <- na.omit(k)
# fix player name
k$Player <- sub(" [A-Z]{2,3}.*", "", k$Player)

dst <- readHTMLTable(getURL("https://www.fantasypros.com/nfl/rankings/dst.php"),
                     stringsAsFactors = F)[["data"]]
# retain a subset of columns
dst <- dst[, c(1:3, 6:7)]
# set column names
names(dst) <- c("Rank", "Player", "Opp", "rankAvg", "rankSD")
# set position
dst$Pos <- "DST"
# remove rows with NA
dst <- na.omit(dst)
# fix player name
dst$Player <- sub(" [A-Z]{2,3}.*", "", dst$Player)

# compile rankings table
rankings <- rbind(qb, rb, wr, te, k, dst)
# join player pool and rankings table
pool <- full_join(pool, rankings, by = c("Player", "Pos"))
# remove rows with NA
pool <- na.omit(pool)

# save file
fantasypros <- pool
save(fantasypros, file = paste0("./Data/Week ", week, "/", sFlag,
                                "/fantasypros.RData"))
write.csv(fantasypros, file = paste0("./Data/Week ", week, "/", sFlag,
                                     "/fantasypros.csv"), row.names = F)