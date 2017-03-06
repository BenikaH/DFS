# download projections from Rotowire
if (sFlag == "FD") {
  site <- "FanDuel"
} else if (sFlag == "DK") {
  site <- "DraftKings"
}
pool <- readHTMLTable(paste0("http://www.rotowire.com/daily/nba/value-report.php?site=",
                             site), stringsAsFactors = F)[[1]]

# select relevant columns
pool <- pool[-nrow(pool), c(1, 7)]
# set column names
names(pool) <- c("Player", "Rotowire")
# standardize player names
pool$Player <- sub("\\W+(GTD|OUT)$", "", pool$Player)
pool$Player <- sapply(pool$Player, replaceName)
# coerce projections as numeric
pool$Rotowire <- as.numeric(pool$Rotowire)
# filter out players whose projection is less than 1
pool <- filter(pool, Rotowire > 1)

# # change abbreviation for New Orleans and Utah
# pool$Team[pool$Team == "NOP"] <- "NOR"
# pool$Team[pool$Team == "UTA"] <- "UTH"

# # save file
rotowire <- pool
save(rotowire, file = "~/OneDrive/R/NBA/Data/rotowire.RData")
write.csv(rotowire, file = "~/OneDrive/R/NBA/Data/rotowire.csv", row.names = F)