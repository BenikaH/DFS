# NFL.com generates projections in whole numbers, which isn't very useful
# don't use unless they change their projection system

# download projections from nfl.com
baseURL <- "http://fantasy.nfl.com/research/projections?offset="
offset <- seq(1, 276, by = 25)
position <- c("O", "K", "8")
URLs <- paste0(baseURL, offset, "&position=", position[1],
               "&statCategory=projectedStats&statSeason=2015&statType=weekProjectedStats&statWeek=",
               week)
offense <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[1]])})

offset <- seq(1, 26, by = 25)
URLs <- paste0(baseURL, offset, "&position=", position[2],
               "&statCategory=projectedStats&statSeason=2015&statType=weekProjectedStats&statWeek=",
               week)
k <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[1]])})

URLs <- paste0(baseURL, offset, "&position=", position[3],
               "&statCategory=projectedStats&statSeason=2015&statType=weekProjectedStats&statWeek=",
               week)
dst <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[1]])})

# offense table
offense <- bind_rows(offense)
offense <- offense[, -c(2, 10, 13)]
names(offense) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD", "recYd", "recTD", "2PT", "Fum")
offense[, -1] <- sapply(offense[, -1], as.numeric)
offense[is.na(offense)] <- 0
offense$FP <- as.matrix(offense[, -1]) %*% as.numeric(scoring[names(offense)[-1]])
offense$Pos <- sub("\\s", "", str_extract(offense$Player, "\\sQB|\\sRB|\\sWR|\\sTE"))

# kicker table
k <- bind_rows(k)
k <- k[, c(1, 9)]
names(k) <- c("Player", "FP")
k[, -1] <- sapply(k[, -1], as.numeric)
k[is.na(k)] <- 0
k <- na.omit(k)
k <- k %>% filter(FP != 0)
k$Pos <- "K"

# defense table
dst <- bind_rows(dst)
dst <- dst[, -c(2, 8, 10)]
names(dst) <- c("Player", "Sack", "DefInt", "FumRec", "Safety", "DefTD", "PA")
dst[, -1] <- sapply(dst[, -1], as.numeric)
dst[is.na(dst)] <- 0

# function for calculating fantasy points from defensive points allowed
calcPA_FP <- function(x) {
  if (x == 0) {
    PA_FP <- 10
  } else if (x >= 1 & x <= 6) {
    PA_FP <- 7
  } else if (x >= 7 & x <= 13) {
    PA_FP <- 4
  } else if (x >= 14 & x <= 20) {
    PA_FP <- 1
  } else if (x >= 21 & x <= 27) {
    PA_FP <- 0
  } else if (x >= 28 & x <= 34) {
    PA_FP <- -1
  } else if (x >= 35) {
    PA_FP <- -4
  }
  return(PA_FP)
}

dst$FP <- as.matrix(dst[, 2:6]) %*% as.numeric(scoring[names(dst)[2:6]]) + sapply(round(dst$PA), calcPA_FP)
dst$Pos <- "DST"

# player pool
pool <- bind_rows(offense, k, dst)
# rearrange columns
pool <- cbind(pool[, "Player"], pool[, c("Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub(" [A-Z]{1,2} - .*", "", pool$Player)
pool$Player <- sub(" DEF.*", "", pool$Player)

# save file
nfl <- pool
save(nfl, file = paste0("./Data/Week ", week, "/nfl.RData"))
write.csv(nfl, file = paste0("./Data/Week ", week, "/nfl.csv"), row.names = F)