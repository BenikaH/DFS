# download projections from ESPN
pool <- readHTMLTable(paste0("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",
                             week, "&seasonId=2016"), stringsAsFactors = F)$playertable_0
for (i in seq(40, 320, by = 40)) {
  pool_i <- readHTMLTable(paste0("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",
                                 week, "&seasonId=2016&startIndex=", i), stringsAsFactors = FALSE)$playertable_0
  pool <- rbind(pool, pool_i[-1, ])
}

# remove header row
pool <- pool[-1, ]
# remove unnecessary columns
pool <- pool[, c(-2, -3, -4, -8)]
# set column names
names(pool) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD", "Rec", "recYd", "recTD", "FP")
# set numeric columns
pool[, -1] <- sapply(pool[, -1], as.numeric)
# set player position
teamPos <- sub(".*, ", "", pool$Player)
pool$Pos <- sub("\\s", "", str_extract(teamPos, "\\sQB|\\sRB|\\sWR|\\sTE|\\sK|\\sD/ST"))
pool <- pool %>% mutate(Pos = ifelse(Pos == "D/ST", "DST", Pos))

# calculate fantasy points for offensive players
offense <- pool %>% filter(Pos %in% c("QB", "RB", "WR", "TE"))
offense$FP <- as.numeric(as.matrix(offense[, -c(1, 10, 11)]) %*% as.numeric(scoring[names(offense)[-c(1, 10, 11)]]))

# player pool
k_dst <- pool %>% filter(Pos %in% c("K", "DST"))
pool <- rbind(offense, k_dst)
# sort player pool by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub(",.*", "", pool$Player)
pool$Player <- sub("\\*", "", pool$Player)
pool$Player <- gsub("\\sD/ST", "", pool$Player)
# rearrange columns
pool <- cbind(pool[, "Player"], pool[, c("Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
colnames(pool)[1] <- "Player"

# save file
espn <- pool
save(espn, file = paste0("./Data/Week ", week, "/", sFlag, "/espn.RData"))
write.csv(espn, file = paste0("./Data/Week ", week, "/", sFlag, "/espn.csv"),
          row.names = F)