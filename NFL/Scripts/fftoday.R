# FFtoday generates projections in whole numbers, which isn't very useful
# don't use unless they change their projection system

# QB table
qb <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week),
                    stringsAsFactors = F)[[11]]
qb <- qb[-1, c(2, 7, 8, 9, 11, 12)]
names(qb) <- c("Player", "passYd", "passTD", "Int", "rushYd", "rushTD")
qb[, -1] <- sapply(qb[, -1], as.numeric)
qb$FP <- as.numeric(as.matrix(qb[, -1]) %*% as.numeric(scoring[names(qb)[-1]]))
qb$Pos <- "QB"

# RB table
rb1 <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                           "&PosID=20&LeagueID=1"), stringsAsFactors = FALSE)[[11]]
rb2 <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                           "&PosID=20&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1"),
                     stringsAsFactors = FALSE)[[11]]
rb <- rbind(rb1, rb2[-1, ])
rb <- rb[-1, c(2, 6, 7, 8, 9, 10)]
names(rb) <- c("Player", "rushYd", "rushTD", "Rec", "recYd", "recTD")
rb[, -1] <- sapply(rb[, -1], as.numeric)
rb$FP <- as.numeric(as.matrix(rb[, -1]) %*% as.numeric(scoring[names(rb)[-1]]))
rb$Pos <- "RB"

# WR table
wr1 <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                            "&PosID=30&LeagueID=1"), stringsAsFactors = FALSE)[[11]]
wr2 <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                            "&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1"),
                     stringsAsFactors = FALSE)[[11]]
wr3 <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                            "&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=2"),
                     stringsAsFactors = FALSE)[[11]]
wr <- rbind(wr1, wr2[-1, ], wr3[-1, ])
wr <- wr[-1, c(2, 5, 6, 7)]
names(wr) <- c("Player", "Rec", "recYd", "recTD")
wr[, -1] <- sapply(wr[, -1], as.numeric)
wr$FP <- as.numeric(as.matrix(wr[, -1]) %*% as.numeric(scoring[names(wr)[-1]]))
wr$Pos <- "WR"

# TE table
te <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                            "&PosID=40&LeagueID=1"), stringsAsFactors = FALSE)[[11]]
te <- te[-1, c(2, 5, 6, 7)]
names(te) <- c("Player", "Rec", "recYd", "recTD")
te[, -1] <- sapply(te[, -1], as.numeric)
te$FP <- as.numeric(as.matrix(te[, -1]) %*% as.numeric(scoring[names(te)[-1]]))
te$Pos <- "TE"

# K table
k <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week,
                           "&PosID=80&LeagueID=1"), stringsAsFactors = FALSE)[[11]]
k <- k[-1, c(2, 5, 7)]
names(k) <- c("Player", "FG", "XP")
k[, -1] <- sapply(k[, -1], as.numeric)
k$FP <- as.numeric(as.matrix(k[, -1]) %*% as.numeric(scoring[names(k)[-1]]))
k$Pos <- "K"

# combine all tables
pool <- bind_rows(qb, rb, wr, te, k)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))
# clean up player names
pool$Player <- sub("\\S\\s", "", pool$Player)

# save file
fftoday <- pool
save(fftoday, file = paste0("./Data/Week ", week, "/fftoday.RData"))
write.csv(fftoday, file = paste0("./Data/Week ", week, "/fftoday.csv"), row.names = F)