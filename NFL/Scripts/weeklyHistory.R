# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("XML")
library("dplyr")
library("stringr")

load("./Data/Weekly History/TE.RData")

a <- unlist(te[1:32, -(1:5)])
a <- a[!is.na(a)]
plot(density(a))

# for (i in 1:10) {
#   a <- as.numeric(te[i, -(1:5)])
#   a <- a[!is.na(a)]
#   plot(density(a))
# }

# readTable <- function(x) {
#   y <- data.frame(readHTMLTable(x, stringsAsFactors = F)[[1]][, c(1, 3)])
#   year <- str_extract(str_extract(x, "yr=[0-9]{4}"), "[0-9]{4}")
#   week <- str_extract(str_extract(x, "wk=[0-9]{1,2}"), "[0-9]{1,2}")
#   yearWeek <- paste(year, week, sep = "_")
#   names(y) <- c("Player", yearWeek)
#   y$Player <- sub(", .*", "", y$Player)
#   pos <- str_extract(str_extract(x, "pos=[A-Z]{1,3}"), "[A-Z]{1,3}")
#   y$Pos <- pos
#   if (pos %in% c("QB", "TE", "K", "DST")) {
#     maxPlayers <- 32
#   } else if (pos == "RB") {
#     maxPlayers <- 64
#   } else if (pos == "WR") {
#     maxPlayers <- 96
#   }
#   y <- y[1:maxPlayers, c("Player", "Pos", yearWeek)]
#   return(y)
# }
# 
# # download weekly historical stats from The Football Database
# baseURL <- "http://www.footballdb.com/fantasy-football/index.html?pos="
# year <- as.character(2010:2014)
# week <- as.character(1:16)
# pos <- "K"
# URLs <- paste0(baseURL, pos, "&yr=", rep(year, each = length(week)), "&wk=", week, "&ppr=")
# poolList <- lapply(URLs, readTable)
# 
# pool <- full_join(poolList[[1]], poolList[[2]], by = c("Player", "Pos"))
# for (i in 3:length(URLs)) {
#   pool <- full_join(pool, poolList[[i]], by = c("Player", "Pos"))
# }
# 
# pool <- pool[!is.na(pool$Player), ]
# pool[, -(1:2)] <- sapply(pool[, -(1:2)], as.numeric)
# avg <- round(rowMeans(pool[, -(1:2)], na.rm = T), 2)
# stdev <- round(sapply(data.frame(t(pool[, -(1:2)])), sd, na.rm = T), 2)
# total <- rowSums(pool[, -(1:2)], na.rm = T)
# pool$Avg <- avg
# pool$SD <- stdev
# pool$Total <- total
# pool <- cbind(pool[, c("Player", "Pos", "Avg", "SD", "Total")],
#                     pool[, -which(colnames(pool) %in% c("Player", "Pos", "Avg", "SD", "Total"))])
# pool <- arrange(pool, desc(total))
# 
# # save files
# k <- pool
# save(k, file = paste0("./Data/Weekly History/", pos, ".RData"))
# write.csv(k, file = paste0("./Data/Weekly History/", pos, ".csv"), row.names = F)