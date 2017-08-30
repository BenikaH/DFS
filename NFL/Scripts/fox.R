# BE CAREFUL - THIS CODE SEEMS TO PRODUCE INCONSISTENT RESULTS FOR SOME REASON
# CHECK TO MAKE SURE THAT RESULTS LOOK REASONABLE

# download projections from Fox
pages <- 1:12
URLs <- paste0("http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=",
                      pages, "&position=-1&split=4&playerSearchStatus=1")
offense <- lapply(URLs, function(x) {readHTMLTable(x, stringsAsFactors = F)[[1]]})

pages <- 1:2
URLs <- paste0("http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=",
               pages, "&position=64&split=4")
k <- lapply(URLs, function(x) {readHTMLTable(x, stringsAsFactors = F)[[1]]})

URLs <- paste0("http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=",
               pages, "&position=32768&split=4")
dst <- lapply(URLs, function(x) {readHTMLTable(x, stringsAsFactors = F)[[1]]})

# offense table
offense <- bind_rows(offense)
offense <- offense[, c(1, 3, 4, 7, 8, 9, 11, 12, 13, 14, 16)]
names(offense) <- c("Player", "passTD", "passYd", "Int", "rushTD", "rushYd", "recTD", "recYd", "Rec", "2PT", "Fum")
offense[, -1] <- sapply(offense[, -1], as.numeric)
offense$FP <- (as.matrix(offense[, -1]) %*% as.numeric(scoring[names(offense)[-1]]))[, 1]
offense$Pos <- sub("\\s", "", str_extract(offense$Player, "\\sQB|\\sRB|\\sWR|\\sTE"))

# kicker table
k <- bind_rows(k)
k <- k[, c(1, 3, 5)]
names(k) <- c("Player", "FG", "XP")
k[, -1] <- sapply(k[, -1], as.numeric)
k$FP <- (as.matrix(k[, -1]) %*% as.numeric(scoring[names(k)[-1]]))[, 1]
k <- na.omit(k)
k <- k %>% filter(FP != 0)
k$Pos <- "K"

# defense table
dst <- bind_rows(dst)
dst <- dst[, c(1, 3:8)]
names(dst) <- c("Player", "DefTD", "FumRec", "Sack", "DefInt", "Safety", "PA")
dst[, -1] <- sapply(dst[, -1], as.numeric)
dst$Player[grep("NYJ", dst$Player)] <- "New York Jets"
dst$Player[grep("NYG", dst$Player)] <- "New York Giants"

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

# remove defenses on bye
dst <- na.omit(dst)

dst$FP <- (as.matrix(dst[, 2:6]) %*% as.numeric(scoring[names(dst)[2:6]]))[, 1] + sapply(round(dst$PA), calcPA_FP)
dst$Pos <- "DST"

# player pool
pool <- bind_rows(offense, k, dst)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub("\\s*\\(.*", "", pool$Player)

# save file
fox <- pool
save(fox, file = paste0("./Data/Week ", week, "/", sFlag, "/fox.RData"))
write.csv(fox, file = paste0("./Data/Week ", week, "/", sFlag, "/fox.csv"),
          row.names = F)