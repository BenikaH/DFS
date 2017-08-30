# download projections from numberFire
offense <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections"),
                      stringsAsFactors = F)
offense <- cbind(offense[[1]], offense[[2]])

# qb confidence intervals
qb <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/qb"),
                         stringsAsFactors = F)
qb <- cbind(qb[[1]], qb[[2]])
qb <- qb[, c(1, 3)]
names(qb) <- c("Player", "CI")

# rb confidence intervals
rb <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/rb"),
                    stringsAsFactors = F)
rb <- cbind(rb[[1]], rb[[2]])
rb <- rb[, c(1, 3)]
names(rb) <- c("Player", "CI")

# wr confidence intervals
wr <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/wr"),
                    stringsAsFactors = F)
wr <- cbind(wr[[1]], wr[[2]])
wr <- wr[, c(1, 3)]
names(wr) <- c("Player", "CI")

# te confidence intervals
te <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/te"),
                    stringsAsFactors = F)
te <- cbind(te[[1]], te[[2]])
te <- te[, c(1, 3)]
names(te) <- c("Player", "CI")

# select relevant rows from offense table
offense <- offense[, c(1, 2, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17,
                       19, 20, 22, 23, 25, 26, 28, 29, 31, 32)]
names(offense) <- c("Player", "NF_FP", "passYd", "passTD", "Int", "rushYd",
                    "rushTD", "Rec", "recYd", "recTD", "FD_FP", "FD_Cost",
                    "DK_FP", "DK_Cost", "DD_FP", "DD_Cost", "FA_FP", "FA_Cost",
                    "FS_FP", "FS_Cost", "Y_FP", "Y_Cost")
# remove dollar sign from cost columns
costSeq <- seq(12, ncol(offense), by = 2)
offense[, costSeq] <- sapply(offense[, costSeq], function(x) {sub("\\$", "", x)})
# set numeric columns
offense[, -1] <- sapply(offense[, -1], as.numeric)
# fantasy points
offense$FP <- as.numeric(as.matrix(offense[, 3:10]) %*% as.numeric(scoring[names(offense)[3:10]]))
# position
offense$Pos <- sub("\\(", "", str_extract(offense$Player, "\\(QB|\\(RB|\\(WR|\\(TE"))

# offensive confidence intervals
ci <- rbind(qb, rb, wr, te)
# lower bound
ci$Low <- as.numeric(sub("-$", "", str_extract(ci$CI, "-*.+-")))
# upper bound
ci$High <- as.numeric(sub(".+-", "", ci$CI))
# remove confidence interval column
ci <- ci[, -which(names(ci) == "CI")]

# add lower and upper bounds to offense table
offense <- inner_join(offense, ci, by = "Player")

# kicker table
k <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k"),
                   stringsAsFactors = F)
k <- cbind(k[[1]], k[[2]])
k <- k[, c(1, 2, 3, 7, 9, 15, 16, 18, 19, 21, 22, 24, 25, 27, 28, 30, 31)]
names(k) <- c("Player", "NF_FP", "CI", "XP", "FG", "FD_FP", "FD_Cost",
              "DK_FP", "DK_Cost", "DD_FP", "DD_Cost", "FA_FP", "FA_Cost",
              "FS_FP", "FS_Cost", "Y_FP", "Y_Cost")
# remove dollar sign from cost columns
costSeq <- seq(7, ncol(k), by = 2)
k[costSeq] <- sapply(k[costSeq], function(x) {sub("\\$", "", x)})
# lower bound
k$Low <- as.numeric(sub("-$", "", str_extract(k$CI, "-*.+-")))
# upper bound
k$High <- as.numeric(sub(".+-", "", k$CI))
# remove confidence interval column
k <- k[, -which(names(k) == "CI")]
# set numeric columns
k[, -1] <- sapply(k[, -1], as.numeric)
# fantasy points
k$FP <- as.numeric(as.matrix(k[, 3:4]) %*% as.numeric(scoring[names(k)[3:4]]))
# position
k$Pos <- "K"

# defense table
dst <- readHTMLTable(paste0("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d"),
                     stringsAsFactors = F)
dst <- cbind(dst[[1]], dst[[2]])
dst <- dst[, c(1, 2, 3, 7, 9, 10, 11, 12, 13, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29)]
names(dst) <- c("Player", "NF_FP", "CI", "PA", "Sack", "DefInt", "FumRec", "DefTD",
                "FD_FP", "FD_Cost", "DK_FP", "DK_Cost", "DD_FP", "DD_Cost",
                "FA_FP", "FA_Cost", "FS_FP", "FS_Cost", "Y_FP", "Y_Cost")
# remove dollar sign from cost columns
costSeq <- seq(10, ncol(dst), by = 2)
dst[costSeq] <- sapply(dst[costSeq], function(x) {sub("\\$", "", x)})
# lower bound
dst$Low <- as.numeric(sub("-$", "", str_extract(dst$CI, "-*.+-")))
# upper bound
dst$High <- as.numeric(sub(".+-", "", dst$CI))
# remove confidence interval column
dst <- dst[, -which(names(dst) == "CI")]
# set numeric columns
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

# fantasy points
dst$FP <- as.matrix(dst[, 4:7]) %*% as.numeric(scoring[names(dst)[4:7]]) + sapply(round(dst$PA), calcPA_FP)
# position
dst$Pos <- "DST"

# combine offense, kicker, and defense tables
pool <- bind_rows(offense, k, dst)

# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP", "Low", "NF_FP", "High")],
              pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP", "Low", "NF_FP", "High"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub(" \\(.*", "", pool$Player)
pool$Player <- sub(" D/ST.*", "", pool$Player)
pool$Player <- sub(" [A-Z]\\..*", "", pool$Player)
pool$Player <- sub("\\s+$", "", pool$Player)

# save file
numberFire <- pool
save(numberFire, file = paste0("./Data/Week ", week, "/", sFlag,
                               "/numberFire.RData"))
write.csv(numberFire, file = paste0("./Data/Week ", week, "/", sFlag,
                                    "/numberFire.csv"), row.names = F)