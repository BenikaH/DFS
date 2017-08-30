# Fleaflicker generates projections in whole numbers, which isn't very useful
# don't use unless they change their projection system

# QB table
qb1 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=4&tableOffset=0",
                      stringsAsFactors = F)[[1]]
qb2 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=4&tableOffset=20",
                     stringsAsFactors = F)[[1]]
qb <- rbind(qb1[-nrow(qb1), ], qb2[c(-1, -2, -nrow(qb2)), ])
qb <- qb[-(1:2), c(1, 2, 11, 12, 13, 17, 18)]
names(qb) <- c("Player", "Pos", "passYd", "passTD", "Int", "rushYd", "rushTD")
qb[, -(1:2)] <- sapply(qb[, -(1:2)], as.numeric)
qb$FP <- as.numeric(as.matrix(qb[, -(1:2)]) %*% as.numeric(scoring[names(qb)[-(1:2)]]))

# flex table
offset <- seq(20, 240, by = 20)
flex <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=11",
                      stringsAsFactors = F)[[1]]
flex <- flex[-nrow(flex), ]
for (i in 1:length(offset)) {
flexTemp <- readHTMLTable(paste0("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=11&tableOffset=",
                             offset[i]), stringsAsFactors = F)[[1]]
flex <- rbind(flex, flexTemp[c(-1, -2, -nrow(flexTemp)), ])
}
flex <- flex[-(1:2), c(1, 2, 9, 10, 12, 13, 14, 16)]
names(flex) <- c("Player", "Pos", "rushYd", "rushTD", "Rec", "recYd", "recTD", "Fum")
flex[, -(1:2)] <- sapply(flex[, -(1:2)], as.numeric)
flex$FP <- as.numeric(as.matrix(flex[, -(1:2)]) %*% as.numeric(scoring[names(flex)[-(1:2)]]))

# K table
k1 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=16&tableOffset=0",
                     stringsAsFactors = F)[[1]]
k2 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=16&tableOffset=20",
                     stringsAsFactors = F)[[1]]
k <- rbind(k1[-nrow(k1), ], k2[c(-1, -2, -nrow(k2)), ])
k <- k[-(1:2), c(1, 2, 8, 11)]
names(k) <- c("Player", "Pos", "FG", "XP")
k[, -(1:2)] <- sapply(k[, -(1:2)], as.numeric)
k$FP <- as.numeric(as.matrix(k[, -(1:2)]) %*% as.numeric(scoring[names(k)[-(1:2)]]))

# DST table
dst1 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=256&tableOffset=0",
                     stringsAsFactors = F)[[1]]
dst2 <- readHTMLTable("http://www.fleaflicker.com/nfl/leaders?statType=7&sortMode=7&position=256&tableOffset=20",
                     stringsAsFactors = F)[[1]]
dst <- rbind(dst1[-nrow(dst1), ], dst2[c(-1, -2, -nrow(dst2)), ])
dst <- dst[-(1:2), c(1, 2, 12, 13, 15, 16, 18)]
names(dst) <- c("Player", "Pos", "DefInt", "Sack", "FumRec", "DefTD", "PA")
dst[, -(1:2)] <- sapply(dst[, -(1:2)], as.numeric)

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

# fantasy points
dst$FP <- as.matrix(dst[, 3:6]) %*% as.numeric(scoring[names(dst)[3:6]]) + sapply(round(dst$PA), calcPA_FP)
dst$Pos <- "DST"

# combine all tables
pool <- bind_rows(qb, flex, k, dst)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))

# save file
fleaflicker <- pool
save(fleaflicker, file = paste0("./Data/Week ", week, "/fleaflicker.RData"))
write.csv(fleaflicker, file = paste0("./Data/Week ", week, "/fleaflicker.csv"), row.names = F)