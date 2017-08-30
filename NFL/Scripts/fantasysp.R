# base URL
baseURL <- "https://www.fantasysp.com/projections/football/weekly/"

# download QB projections from FantasySP
qb <- readHTMLTable(getURL(paste0(baseURL, "QB")), stringsAsFactors = F, header = F)[[1]]
# select relevant columns
qb <- qb[, c(2, 4, 6, 7, 8, 9)]
names(qb) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD")
# assign position
qb$Pos <- "QB"
# extract team name
qb$Team <- str_extract(qb$Player, "[A-Z]{2,3}")
# remove team name and position from Player column
qb$Player <- sub("[A-Z]{2,3} QB", "", qb$Player)
# rearrange columns
qb <- cbind(qb[, c("Player", "Pos", "Team")], qb[, -which(colnames(qb) %in% c("Player", "Pos", "Team"))])
# set numeric columns
qb[, -(1:3)] <- sapply(qb[, -(1:3)], as.numeric)
# bonus for 300+ passing yards
qb$Bonus <- as.numeric(qb$passYd > 299)
# fantasy points
qb$FP <- as.numeric(as.matrix(qb[, -1]) %*% as.numeric(scoring[names(qb)[-1]]))


# download kicker projections from Yahoo
baseURL <- "https://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=K&cut_type=9&stat1=S_PW_"
offset <- c(0, 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PR&sdir=1&count=", offset)
k <- lapply(URLs, function(x) {data.frame(readHTMLTable(getURL(x), stringsAsFactors = F)[[2]])})
k <- bind_rows(k)

# select relevant columns
k <- k[, c(2, 11:16)]
# set numeric columns
k[, -1] <- sapply(k[, -1], as.numeric)
# total FGs
k <- k %>% mutate(FG = X0.19 + X20.29 + X30.39 + X40.49 + X50.)
# remove FG-by-range columns
k <- k[, -(2:6)]
# set column names
names(k) <- c("Player", "XP", "FG")
# fantasy points
k$FP <- as.numeric(as.matrix(k[, -1]) %*% as.numeric(scoring[names(k)[-1]]))
# position
k$Pos <- "K"
# remove kickers with 0 projected points
k <- k %>% filter(FP != 0)

# download defensive projections from Yahoo
baseURL <- "https://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=DEF&cut_type=9&stat1=S_PW_"
offset <- c(0, 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PR&sdir=1&count=", offset)
dst <- lapply(URLs, function(x) {data.frame(readHTMLTable(getURL(x), stringsAsFactors = F)[[2]])})
dst <- bind_rows(dst)

# select relevant columns
dst <- dst[, c(2, 11:16)]
# set column names
names(dst) <- c("Player", "PA", "Sack", "Safety", "DefInt", "FumRec", "DefTD")
# set numeric columns
dst[, -1] <- sapply(dst[, -1], as.numeric)
# set correct names for Jets and Giants
dst$Player[grep("NYJ - DEF", dst$Player)] <- "New York Jets"
dst$Player[grep("NYG - DEF", dst$Player)] <- "New York Giants"

# function for calculating fantasy points from defensive points allowed
calcPA_FP <- function(x) {
  if (x >= 0 & x <= 1) {
    PA_FP <- 10
  } else if (x > 1 & x <= 20) {
    PA_FP <- (-6/19) * x + 139/19
  } else if (x > 20 & x <= 34) {
    PA_FP <- (-1/7) * x + 27/7
  } else if (x > 34) {
    PA_FP <- -4
  }
  return(PA_FP)
}

# fantasy points
dst$FP <- as.matrix(dst[, -(1:2)]) %*% as.numeric(scoring[names(dst)[-(1:2)]]) + sapply(round(dst$PA), calcPA_FP)
# position
dst$Pos <- "DST"

# combine offense, kicker, and defense tables
pool <- bind_rows(offense, k, dst)

# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub(" [A-Z]([A-Z]|[a-z]){1,2} - .*", "", pool$Player)
pool$Player <- sub("New Player Note(\\s|\\n)*|Player Note(\\s|\\n)*|No new player Notes(\\s|\\n)*|Video Playlist(\\s|\\n)*",
                   "", pool$Player)
pool$Player <- sub("New Player Note(\\s|\\n)*|Player Note(\\s|\\n)*|No new player Notes(\\s|\\n)*|Video Playlist(\\s|\\n)*",
                   "", pool$Player)
# remove players with 0 projected points
pool <- pool %>% filter(FP != 0)

# save file
yahoo <- pool
save(yahoo, file = paste0("./Data/Week ", week, "/", sFlag, "/yahoo.RData"))
write.csv(yahoo, file = paste0("./Data/Week ", week, "/", sFlag, "/yahoo.csv"),
          row.names = F)