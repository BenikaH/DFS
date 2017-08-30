# download projections from Fantasy Sharks
offense <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=ALL&format=csv",
                    stringsAsFactors = F)
k <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=PK&format=csv",
                    stringsAsFactors = F)
dst <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=D&format=csv",
                    stringsAsFactors = F)

# CHECK TO SEE THAT DST TABLE IS FORMATTED CORRECTLY - THERE WAS AN ERROR IN WEEK 2
names(dst)[6:11] <- names(dst)[7:12]
dst <- dst[, -12]

# set offense columns
offense <- offense[, c("Pos", "Name", "Yards", "TD", "Int", "Yards.1", "TD.1", "Rec", "Yards.2", "TD.2")]
names(offense) <- c("Pos", "Player", "passYd", "passTD", "Int", "rushYd", "rushTD", "Rec", "recYd", "recTD")
offense$FP <- as.matrix(offense[, -(1:2)]) %*% as.numeric(scoring[names(offense)[-(1:2)]])

# set kicker columns
k <- k[, c("Name", "XP", "Total")]
names(k) <- c("Player", "XP", "FG")
k[, -1] <- sapply(k[, -1], as.numeric)
k$FP <- as.matrix(k[, 2:3]) %*% as.numeric(scoring[names(k)[-1]])
k$Pos <- "K"

# set defense columns
dst <- dst[, c("Name", "Int", "Fum", "Sack", "Pts.Allow")]
names(dst) <- c("Player", "DefInt", "FumRec", "Sack", "PA")

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

dst$FP <- as.matrix(dst[, 2:4]) %*% as.numeric(scoring[names(dst)[2:4]]) + sapply(round(dst$PA), calcPA_FP)
dst$Pos <- "DST"

# player pool
pool <- bind_rows(offense, k, dst)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# set player name
firstName <- sub(".*, ", "", pool$Player)
lastName <- sub(",.*", "", pool$Player)
pool$Player <- paste(firstName, lastName)

# save file
fantasySharks <- pool
save(fantasySharks, file = paste0("./Data/Week ", week, "/", sFlag,
                                  "/fantasySharks.RData"))
write.csv(fantasySharks, file = paste0("./Data/Week ", week, "/", sFlag,
                                       "/fantasySharks.csv"), row.names = F)