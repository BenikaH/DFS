# position list
pos <- c("PG", "SG", "SF", "PF", "C")

# download DvP from Rotowire
for (i in 1:length(pos)) {
  temp <- readHTMLTable(paste0("http://www.rotowire.com/daily/nba/defense-vspos.php?site=",
                               site, "&astatview=season&pos=", pos[i]), stringsAsFactors = F)[[1]]
  if (i == 1) {
    dvp <- temp
  } else {
    dvp <- rbind(dvp, temp)
  }
}

# retain relevant columns
dvp <- dvp[, 1:3]
# change column name
names(dvp) <- c("Opp", "Pos", "DvP")
# replace team name with abbreviation
dvp$Opp <- sapply(dvp$Opp, teamAbbr)
# convert DvP to numeric
dvp$DvP <- as.numeric(dvp$DvP)

# save file
save(dvp, file = paste0("Data/", sFlag, "/dvp.RData"), row.names = F)