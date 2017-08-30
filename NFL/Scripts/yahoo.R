# download offensive projections from Yahoo
baseURL <- "https://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=O&cut_type=9&stat1=S_PW_"
offset <- seq(0, 325, by = 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PR&sdir=1&count=", offset)
offense <- lapply(URLs, function(x) {data.frame(readHTMLTable(getURL(x), stringsAsFactors = F)[[2]])})
offense <- bind_rows(offense)

# select relevant columns
offense <- offense[, c(2, 11, 12, 13, 15, 16, 17, 18, 19, 22, 23)]
names(offense) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD",
                    "Rec", "recYd", "recTD", "2PT", "Fum")
# set numeric columns
offense[, -1] <- sapply(offense[, -1], as.numeric)
# fantasy points
offense$FP <- as.numeric(as.matrix(offense[, -1]) %*% as.numeric(scoring[names(offense)[-1]]))
# position
offense$Pos <- sub(" ", "", str_extract(offense$Player, " QB| RB| WR| TE"))

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