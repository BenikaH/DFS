# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("XML")
library("RCurl")
library("dplyr")
library("stringr")
library("readxl")
library("xlsx")
library("Rglpk")
library("Rsolnp")
library("ggplot2")

# set working directory
setwd("~/OneDrive/R/NFL")

# scoring flag
sFlag <- "FD"
# sFlag <- "DK"

# load functions and league settings
source("./Scripts/functions2.R")
source("./Scripts/leagueSettings.R")

# week number
week <- 16
# source flag
sourceFlag <- 1
# rotoviz flag
rotoFlag <- 0
# view flag
viewFlag <- 0
# number of simulations
nSim <- 0
# Sharpe probability
sharpeProb <- 0.98
# minimum allowable FantasyPros ranking
minRank <- 25

# create new directories if they doesn't already exist
if (!dir.exists(paste0("./Data/Week ", week))) {
  dir.create(paste0("./Data/Week ", week))
  dir.create(paste0("./Data/Week ", week, "/FD"))
  dir.create(paste0("./Data/Week ", week, "/DK"))
}
if (!dir.exists(paste0("~/Dropbox/DFS/Week ", week))) {
  dir.create(paste0("~/Dropbox/DFS/Week ", week))
  dir.create(paste0("~/Dropbox/DFS/Week ", week, "/FD"))
  dir.create(paste0("~/Dropbox/DFS/Week ", week, "/DK"))
}

# players whose projections are very different from FantasyPros rankings
problemPlayers <- ""

# players to exclude
excludeDF <- read_excel(paste0("./Data/exclude.xlsx"), col_names = T)
excludeDF$Pos <- factor(excludeDF$Pos, levels = c("QB", "RB", "WR", "TE", "K", "DST"))

# list of data sources
sources <- c("cbs", "espn", "fantasySharks", "fox", "numberFire", "yahoo")
# download and save projections from all data sources
if (sourceFlag == 1) {
  for (i in 1:length(sources)) {
    source(paste0("./Scripts/", sources[i], ".R"))
  }
  if (rotoFlag == 1) {
    source("./Scripts/rotoviz.R")
  }
  source("./Scripts/fantasypros.R")
}
# load data frames
for (i in 1:length(sources)) {
  load(paste0("./Data/Week ", week, "/", sFlag, "/", sources[i], ".RData"))
}
load(paste0("./Data/Week ", week, "/", sFlag, "/fantasypros.RData"))

# CBS
cbs <- cbs[, 1:3]
names(cbs)[3] <- "CBS"
cbs$Player <- sapply(cbs$Player, replaceName)

# ESPN
espn <- espn[, 1:3]
names(espn)[c(1, 3)] <- c("Player", "ESPN")
espn$Player <- as.character(espn$Player)
espn$Player <- sapply(espn$Player, replaceName)

# FantasySharks
fantasySharks <- fantasySharks[, 1:3]
names(fantasySharks)[3] <- "FantasySharks"
fantasySharks$Player <- sapply(fantasySharks$Player, replaceName)

# Fox
fox <- fox[, 1:3]
names(fox)[3] <- "Fox"
fox$Player <- sapply(fox$Player, replaceName)

# numberFire
numberFire <- numberFire[, c("Player", "Pos", "FP")]
names(numberFire)[3] <- "numberFire"
numberFire$Player <- sapply(numberFire$Player, replaceName)

# Yahoo
yahoo <- yahoo[, 1:3]
names(yahoo)[3] <- "Yahoo"
yahoo$Player <- sapply(yahoo$Player, replaceName)

# FantasyPros
fantasypros$Player <- sapply(fantasypros$Player, replaceName)
# if FantasyPros DST projections are extremely high, set them to NA
# fantasypros[fantasypros$Pos == "DST", "FantasyPros"] <- NA

# join data frames
pool <- full_join(cbs, espn, by = c("Player", "Pos")) %>%
        full_join(fantasySharks, by = c("Player", "Pos")) %>%
        full_join(fox, by = c("Player", "Pos")) %>%
        full_join(numberFire, by = c("Player", "Pos")) %>%
        full_join(yahoo, by = c("Player", "Pos")) %>%
        full_join(fantasypros, by = c("Player", "Pos"))

# rotoviz
if (rotoFlag == 1) {
  load(paste0("./Data/Week ", week, "/rotoviz.RData"))
  rotoviz <- rotoviz[, 1:3]
  names(rotoviz)[3] <- "rotoviz"
  rotoviz$Player <- sapply(tolower(rotoviz$Player), simpleCap)
  rotoviz$Player <- sapply(rotoviz$Player, replaceName)
  pool <- full_join(pool, rotoviz, by = c("Player", "Pos"))
}

# views
if (viewFlag == 1) {
  views <- read_excel(paste0("~/Dropbox/FanDuel/views.xlsx"), col_names = T)
  pool <- full_join(pool, views, by = c("Player", "Pos"))
}

# remove players from the exclude list
pool <- pool %>% filter(!(Player %in% excludeDF$Player))
pool <- pool %>% filter(!(Player %in% problemPlayers))
# rearrange columns
pool <- cbind(pool[, c("Player", "Team", "Pos", "Kickoff", "Opp", "Cost", "Rank", "rankAvg", "rankSD")],
              pool[, -which(colnames(pool) %in% c("Player", "Team", "Pos", "Kickoff", "Opp", "Cost",
                                                  "Rank", "rankAvg", "rankSD"))])
# median projected points
poolMed <- sapply(data.frame(t(pool[, -(1:9)])), mean, na.rm = T)
# standard error of projected points
poolSE <- sapply(data.frame(t(pool[, -(1:9)])), calcSE)
# store Med and SE in player pool
pool$Projection <- poolMed
pool$SE <- poolSE
# round all numbers to 2 decimal points
pool[, -(1:9)] <- sapply(pool[, -(1:9)], round, digits = 2)
# remove players from games prior to Sun 1:00PM
pool <- pool %>% filter(Kickoff != "Thu 8:25PM" & Kickoff != "Sun 9:30AM" &
                        Kickoff != "Mon 8:30PM")
# filter out players with low FantasyPros rankings
pool$Rank <- as.numeric(pool$Rank)
pool <- pool[(pool$Pos == "QB" & pool$Rank <= minRank + table(excludeDF$Pos)["QB"]) |
             (pool$Pos == "RB" & pool$Rank <= 2*minRank + table(excludeDF$Pos)["RB"]) |
             (pool$Pos == "WR" & pool$Rank <= 3*minRank + table(excludeDF$Pos)["WR"]) |
             (pool$Pos == "TE" & pool$Rank <= minRank + table(excludeDF$Pos)["TE"]) |
             (pool$Pos == "K" & pool$Rank <= minRank + table(excludeDF$Pos)["K"]) |
             (pool$Pos == "DST" & pool$Rank <= minRank + table(excludeDF$Pos)["DST"]), ]
# difference between FantasyPros rank and projection rank
for (pos in c("QB", "RB", "WR", "TE", "K", "DST")) {
  pool$rankDiff[pool$Pos == pos] <- pool$Rank[pool$Pos == pos] - rank(-pool$Projection[pool$Pos == pos])
}
# rearrange columns
pool <- cbind(pool[, c("Player", "Team", "Pos", "Kickoff", "Opp", "Cost",
                       "Projection", "SE", "Rank", "rankDiff", "rankAvg",
                       "rankSD")], pool[, -which(colnames(pool) %in% c("Player",
                       "Team", "Pos", "Kickoff", "Opp", "Cost", "Projection",
                       "SE", "Rank", "rankDiff", "rankAvg", "rankSD"))])
# sort player pool by median projected points
pool <- arrange(pool, desc(Projection))

# create column for player last names
pool$Last[pool$Pos != "DST"] <- sub("^\\S+ ", "", pool$Player[pool$Pos != "DST"])
pool$Last[pool$Pos == "DST"] <- str_extract(pool$Player[pool$Pos == "DST"],
                                            " \\w+$")

# eval_f <- function(x, a, b) {
#   y <- sapply(data.frame(t(cbind(x, b))), mean, na.rm = T)
#   sum(abs(rank(-y) - a))
# }
# 
# pool$rankView <- NA
# for (pos in c("QB", "RB", "WR", "TE", "K", "DST")) {
#   minProj <- sapply(data.frame(t(pool[pool$Pos == pos, 13:20])), min, na.rm = T)
#   maxProj <- sapply(data.frame(t(pool[pool$Pos == pos, 13:20])), max, na.rm = T)
#   res <- nloptr(x0 = pool$Med[pool$Pos == pos], eval_f = eval_f, lb = minProj, ub = maxProj,
#                 opts = list("algorithm"="NLOPT_LN_COBYLA"), a = pool$Rank[pool$Pos == pos],
#                 b = pool[pool$Pos == pos, 13:20])
#   pool$rankView[pool$Pos == pos] <- res$solution
# }
# 
# pool$Med <- sapply(data.frame(t(pool[, 13:21])), mean, na.rm = T)

# save player pool
save(pool, file = paste0("./Data/Week ", week, "/", sFlag, "/playerPool.RData"))
write.csv(pool, file = paste0("./Data/Week ", week, "/", sFlag,
                              "/playerPool.csv"), row.names = F)
write.csv(pool, file = paste0("~/Dropbox/DFS/Week ", week,
                              "/", sFlag, "/playerPool.csv"), row.names = F)
write.xlsx(pool, file = paste0("./Data/Week ", week, "/", sFlag,
                               "/playerPool.xlsx"), row.names = F)

# scatterplots for each position
plots <- list()
if (sFlag == "FD") {
  pos <- c("QB", "RB", "WR", "TE", "K", "DST")
} else if (sFlag == "DK") {
  pos <- c("QB", "RB", "WR", "TE", "DST")
}
for (i in 1:length(pos)) {
  plots[[i]] <- ggplot(pool %>% filter(Pos == pos[i]),
                       aes(x = Cost, y = Projection, label = Last)) +
                geom_jitter() +
                geom_text(size = 2, vjust = -0.75) +
                geom_smooth(method = "lm") +
                ggtitle(pos[i])
  print(plots[[i]])
  ggsave(paste0("~/Dropbox/DFS/Week ", week, "/", sFlag, "/",
                pos[i], ".jpg"), width = 5, height = 5)
  dev.off()
}

# # most probable optimal team
# mostProbable <- optimizeTeam(pool$Projection, pool[, 1:10], constraints)
# mostProbable <- mostProbable %>% filter(Binary == 1) %>% select(-Binary)
# 
# # most probable optimal team 2
# # mostProbable2 <- fmincon(pool$Projection, pool[, 1:10], constraints)
# # mostProbable2 <- mostProbable2 %>% filter(Binary == 1) %>% select(-Binary)
# 
# # save most probable optimal team
# save(mostProbable, file = paste0("./Data/Week ", week, "/mostProbable.RData"))
# write.csv(mostProbable, file = paste0("./Data/Week ", week, "/mostProbable.csv"), row.names = F)
# write.csv(mostProbable, file = "C:/Users/Matt/Dropbox/FanDuel/mostProbable.csv", row.names = F)
# 
# if (nSim > 0) {
#   # simulated player points
#   simPoints <- data.frame(t(mapply(rnorm, n = nSim, mean = pool$Projection, sd = pool$SE)))
#   # simulated optimal teams
#   simTeams <- lapply(simPoints, optimizeTeam, playerInfo = pool[, 1:10], constraints = constraints)
#   # tabulate which players are used in each simulation
#   simBinary <- simTeams[[1]]$Binary
#   for (i in 2:nSim) {
#     simBinary <- cbind(simBinary, simTeams[[i]]$Binary)
#   }
#   # points scored by players on optimal teams
#   pts <- sapply(simPoints * simBinary, sum)
#   # total risk of each optimal team
# #   risk <- log10(1 / sapply(data.frame(sapply(simPoints, dnorm, mean = pool$Projection, sd = pool$SE)) * simBinary,
# #                            function(x) {prod(x[x != 0], na.rm = T)}))
#   covMat <- diag(pool$SE)
#   risk <- apply(simBinary, 2, function(x, S) {as.numeric(x %*% S %*% x)}, S = covMat)
#   # total risk of most probable team
#   mpRisk <- log10(1 / prod(dnorm(mostProbable$Projection, mean = pool$Projection[pool$Player %in% mostProbable$Player],
#                                  sd = pool$SE[pool$Player %in% mostProbable$Player])))
# #   # Sharpe ratio
# #   sharpe <- pts / risk
# #   # Sharpe ratio cutoff
# #   cutoff <- quantile(sharpe, sharpeProb)
# #   # which teams exceed the cutoff
# #   best <- which(sharpe > cutoff)
#   
#   best <- NA
#   riskSeq <- seq(from = min(risk), to = max(risk), length.out = nSim * (1 - sharpeProb))
#   for (i in 1:length(riskSeq) - 1) {
#     if (length(risk[risk >= riskSeq[i] & risk <= riskSeq[i + 1]]) > 0) {
#       best[i] <- which(pts == max(pts[risk >= riskSeq[i] & risk <= riskSeq[i + 1]]))
#     }
#   }
#   best <- best[!is.na(best)]
#   
#   # calculate player exposure on the best lineups
#   exposure <- cbind(pool[, 1:10], round(rowSums(simBinary[, best]) / length(best), 2))
#   names(exposure)[11] <- "Exposure"
#   exposure <- exposure %>% arrange(desc(Exposure))
#   
#   # save player exposure
#   save(exposure, file = paste0("./Data/Week ", week, "/exposure.RData"))
#   write.csv(exposure, file = paste0("./Data/Week ", week, "/exposure.csv"), row.names = F)
#   write.csv(exposure, file = "C:/Users/Matt/Dropbox/FanDuel/exposure.csv", row.names = F)
#   
#   # format list of best lineups to match FanDuel standard
#   # lineup ordered as it appears on FanDuel (QB, RB, RB, WR, WR, WR, TE, K, DST)
#   playerPos <- pool[simBinary[, best[1]] == 1, c("Player", "Pos")]
#   QB <- playerPos$Player[playerPos$Pos == "QB"]
#   RB <- playerPos$Player[playerPos$Pos == "RB"]
#   WR <- playerPos$Player[playerPos$Pos == "WR"]
#   TE <- playerPos$Player[playerPos$Pos == "TE"]
#   K <- playerPos$Player[playerPos$Pos == "K"]
#   DST <- playerPos$Player[playerPos$Pos == "DST"]
#   # bestLineups <- c(QB, RB, WR, TE, K, DST, sharpe[best[1]])
#   bestLineups <- c(QB, RB, WR, TE, K, DST, pts[best[1]])
#   for (i in 2:length(best)) {
#     playerPos <- pool[simBinary[, best[i]] == 1, c("Player", "Pos")]
#     QB <- playerPos$Player[playerPos$Pos == "QB"]
#     RB <- playerPos$Player[playerPos$Pos == "RB"]
#     WR <- playerPos$Player[playerPos$Pos == "WR"]
#     TE <- playerPos$Player[playerPos$Pos == "TE"]
#     K <- playerPos$Player[playerPos$Pos == "K"]
#     DST <- playerPos$Player[playerPos$Pos == "DST"]
#     # bestLineups <- rbind(bestLineups, c(QB, RB, WR, TE, K, DST, sharpe[best[i]]))
#     bestLineups <- rbind(bestLineups, c(QB, RB, WR, TE, K, DST, pts[best[i]]))
#   }
#   bestLineups <- data.frame(bestLineups)
#   # order list of best lineups by Sharpe ratio
#   names(bestLineups)[10] <- "Sharpe"
#   bestLineups <- bestLineups %>% arrange(desc(Sharpe))
#   bestLineups <- bestLineups[, -10]
#   names(bestLineups) <- NULL
#   
#   # plot(risk[risk < 6], pts[risk < 6], type = "p")
#   # points(risk[best], pts[best], col = "red")
#   # points(mpRisk, sum(mostProbable$Points), col = "green")
#   # abline(lm(pts[risk < 6] ~ risk[risk < 6]), col = "blue")
#   plot(risk, pts, type = "p")
#   points(risk[best], pts[best], col = "red")
#   # points(mpRisk, sum(mostProbable$Points), col = "green")
#   abline(lm(pts ~ risk), col = "blue")
#   # plot(density(pts))
#   
#   # save list of best lineups
#   save(bestLineups, file = paste0("./Data/Week ", week, "/bestLineups.RData"))
#   write.csv(bestLineups, file = paste0("./Data/Week ", week, "/bestLineups.csv"), row.names = F)
#   write.csv(bestLineups, file = "C:/Users/Matt/Dropbox/FanDuel/bestLineups.csv", row.names = F)
# }