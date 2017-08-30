# download projections from CBS
baseURL <- "http://www.cbssports.com/fantasy/football/stats/weeklyprojections/"
position <- c("QB","RB","WR","TE","K","DST")
URLs <- paste0(baseURL, position, "/", week, "/avg/standard?&print_rows=9999")
poolList <- lapply(URLs, function(x) {data.frame(readHTMLTable(x, stringsAsFactors = F)[[1]])})

# create data frames for each position
qb <- poolList[[1]]
rb <- poolList[[2]]
wr <- poolList[[3]]
te <- poolList[[4]]
k <- poolList[[5]]
dst <- poolList[[6]]

# QB table
qb <- qb[c(-1, -2, -nrow(qb)), c(1, 4, 5, 6, 10, 12, 13)]
names(qb) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD", "Fum")
qb[, -1] <- sapply(qb[, -1], as.numeric)
qb$FP <- as.matrix(qb[, 2:7]) %*% as.numeric(scoring[names(qb)[-1]])
qb$Pos <- "QB"

# RB table
rb <- rb[c(-1, -2, -nrow(rb)), c(1, 3, 5, 6, 7, 9, 10)]
names(rb) <- c("Player", "rushYd", "rushTD", "Rec", "recYd", "recTD", "Fum")
rb[, -1] <- sapply(rb[, -1], as.numeric)
rb$FP <- as.matrix(rb[, 2:7]) %*% as.numeric(scoring[names(rb)[-1]])
rb$Pos <- "RB"

# WR table
wr <- wr[c(-1, -2, -nrow(wr)), c(1, 2, 3, 5, 6)]
names(wr) <- c("Player", "Rec", "recYd", "recTD", "Fum")
wr[, -1] <- sapply(wr[, -1], as.numeric)
wr$FP <- as.matrix(wr[, 2:5]) %*% as.numeric(scoring[names(wr)[-1]])
wr$Pos <- "WR"

# TE table
te <- te[c(-1, -2, -nrow(te)), c(1, 2, 3, 5, 6)]
names(te) <- c("Player", "Rec", "recYd", "recTD", "Fum")
te[, -1] <- sapply(te[, -1], as.numeric)
te$FP <- as.matrix(te[, 2:5]) %*% as.numeric(scoring[names(te)[-1]])
te$Pos <- "TE"

# K table
k <- k[-1, c(1, 2, 4)]
names(k) <- c("Player", "FG", "XP")
k[, -1] <- sapply(k[, -1], as.numeric)
k$FP <- as.matrix(k[, 2:3]) %*% as.numeric(scoring[names(k)[-1]])
k$Pos <- "K"

# DST table
dst <- dst[-1, c(1, 2, 3, 5, 6, 7, 8)]
names(dst) <- c("Player", "DefInt", "FumRec", "Sack", "DefTD", "Safety", "PA")
dst[, -1] <- sapply(dst[, -1], as.numeric)

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

dst$FP <- as.matrix(dst[, 2:6]) %*% as.numeric(scoring[names(dst)[c(-1, -ncol(dst))]]) +
          sapply(round(dst$PA), calcPA_FP)
dst$Pos <- "DST"

# player pool
pool <- bind_rows(qb, rb, wr, te, k, dst)
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
pool <- arrange(pool, desc(FP))
pool$Player <- sub(",.*", "", pool$Player)
pool <- pool %>% filter(FP != 0)

# save file
cbs <- pool
save(cbs, file = paste0("./Data/Week ", week, "/", sFlag, "/cbs.RData"))
write.csv(cbs, file = paste0("./Data/Week ", week, "/", sFlag, "/cbs.csv"),
          row.names = F)