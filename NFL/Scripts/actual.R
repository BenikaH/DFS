# # set week number
# week <- 1
# 
# # scoring system
# scoring <- data.frame(t(c(0.04, 4, -1, 0.1, 6, -2, 0.5, 0.1, 6, 2, 3, 1, 2, 2, 1, 6, 2)))
# names(scoring) <- c("passYd", "passTD", "Int", "rushYd", "rushTD", "Fum", "Rec", "recYd", "recTD", "2PT",
#                     "FG", "XP", "DefInt", "FumRec", "Sack", "DefTD", "Safety")

# download weekly results from Yahoo
baseURL <- "http://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=O&cut_type=9&stat1=S_W_"
offset <- seq(0, 325, by = 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PTS&sdir=1&count=", offset)
offense <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[2]])})
offense <- bind_rows(offense)

# select relevant columns
offense <- offense[, c(2, 11, 12, 13, 15, 16, 17, 18, 19, 22, 23)]
names(offense) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD", "Rec", "recYd", "recTD", "2PT", "Fum")
# set numeric columns
offense[, -1] <- sapply(offense[, -1], as.numeric)
# replace NA with zero
offense[is.na(offense)] <- 0
# fantasy points
offense$Actual <- as.numeric(as.matrix(offense[, -1]) %*% as.numeric(scoring[names(offense)[-1]]))
# position
offense$Pos <- sub(" ", "", str_extract(offense$Player, " QB| RB| WR| TE"))

# download kicker results from Yahoo
baseURL <- "http://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=K&cut_type=9&stat1=S_W_"
offset <- c(0, 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PTS&sdir=1&count=", offset)
k <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[2]])})
k <- bind_rows(k)

# select relevant columns
k <- k[, c(2, 7)]
# set numeric columns
k[, -1] <- sapply(k[, -1], as.numeric)
# set column names
names(k) <- c("Player", "Actual")
# position
k$Pos <- "K"

# download defensive results from Yahoo
baseURL <- "http://football.fantasysports.yahoo.com/f1/607500/players?status=ALL&pos=DEF&cut_type=9&stat1=S_W_"
offset <- c(0, 25)
URLs <- paste0(baseURL, week, "&myteam=0&sort=PTS&sdir=1&count=", offset)
dst <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[2]])})
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
# remove teams on bye week
dst <- na.omit(dst)

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
dst$Actual <- as.matrix(dst[, -(1:2)]) %*% as.numeric(scoring[names(dst)[-(1:2)]]) + sapply(round(dst$PA), calcPA_FP)
# position
dst$Pos <- "DST"

# combine offense, kicker, and defense tables
pool <- bind_rows(offense, k, dst)

# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "Actual")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "Actual"))])
# sort by fantasy points
pool <- arrange(pool, desc(Actual))
# clean up player names
pool$Player <- sub(" [A-Z]([A-Z]|[a-z]){1,2} - .*", "", pool$Player)
pool$Player <- sub("New Player Note(\\s|\\n)*|Player Note(\\s|\\n)*|No new player Notes(\\s|\\n)*|Video Playlist(\\s|\\n)*",
                   "", pool$Player)
pool$Player <- sub("New Player Note(\\s|\\n)*|Player Note(\\s|\\n)*|No new player Notes(\\s|\\n)*|Video Playlist(\\s|\\n)*",
                   "", pool$Player)
# remove players with 0 projected points
pool <- pool %>% filter(Actual != 0)

# save file
actual <- pool
save(actual, file = paste0("./Data/Week ", week, "/", sFlag, "/actual.RData"))
write.csv(actual, file = paste0("./Data/Week ", week, "/", sFlag, "/actual.csv"),
          row.names = F)