# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("XML")
library("dplyr")
library("stringr")
library("Rglpk")
library("ggplot2")

# league settings
nQB <- 1
nRB <- 2
nWR <- 3
nTE <- 1
nK <- 1
nDST <- 1
budget <- 60000
nStarters <- 9

# download FanDuel DFS player pool
pool <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-lineup-optimizer.php", stringsAsFactors = FALSE)[["player-pool"]]
# retain a subset of columns
pool <- pool[, c(1:2, 4:6)]
# set column names
names(pool) <- c("Player", "PosRank", "Kickoff", "Points", "Salary")
# remove rows with unranked players
pool <- pool %>% filter(PosRank != "NR")

# player name
pool$Name <- sub(",.*", "", pool$Player)
# first name
pool$First <- sapply(strsplit(pool$Name, " "), '[[', 1)
# last name
pool$Last <- sapply(strsplit(pool$Name, " "), '[[', 2)
# team name
teams <- c("DAL", "NE", "BAL", "SEA", "ATL", "GB", "IND", "PHI", "DEN", "NYG", "DET", "NO", "SD", "MIN", "PIT",
           "ARI", "CHI", "BUF", "SF", "MIA", "CIN", "STL", "KC", "NYJ", "CAR", "OAK", "HOU", "TEN", "CLE",
           "TB", "WAS", "JAC")
temp <- sapply(pool$Player, str_extract, pattern = teams)
pool$Team <- temp[!is.na(temp)]
# position
pool$Position <- sapply(strsplit(pool$PosRank, " "), '[[', 1)
# position rank
pool$Rank <- sapply(strsplit(pool$PosRank, " "), '[[', 2)
pool$Rank <- sub("#", "", pool$Rank)
# salary
pool$Salary <- sub("\\$", "", pool$Salary)
pool$Salary <- sub(",", "", pool$Salary)

# retain a subset of columns
pool <- pool[c("Name", "First", "Last", "Team", "Position", "Rank", "Kickoff", "Points", "Salary")]
# set position as factor
pool$Position <- as.factor(pool$Position)
# change class of numeric columns
pool[, c("Rank", "Points", "Salary")] <- sapply(pool[, c("Rank", "Points", "Salary")], as.numeric)
# sort player pool by projected points
pool <- pool %>% arrange(desc(Points))
# filter by game time
# pool <- pool %>% filter(Kickoff == "Sun 1:00PM")

# set team names for team defenses
# rankings[rankings$Position == "DST", ]$Team <- merge(filter(rankings, Position == "DST"),
#                                                      filter(pool, Position == "DST"), by = "Name")$Team.y

# optimizer
optimizeTeam <- function(pool, nQB, nRB, nWR, nTE, nK, nDST, budget, nStarters){
  # number of players in the pool
  n <- length(pool$Name)
  # set variable types to "binary" for solver
  var.types <- rep("B", n)
  
  A <- rbind(as.numeric(pool$Position == "QB"),
             as.numeric(pool$Position == "RB"),
             as.numeric(pool$Position == "WR"),
             as.numeric(pool$Position == "TE"),
             as.numeric(pool$Position == "K"),
             as.numeric(pool$Position == "DST"),
             #optimizeData$risk,
             #diag(optimizeData$risk),
             pool$Salary,
             rep(1, n))   
  
  dir <- c("==",
           ">=",
           ">=",
           ">=",
           "==",
           "==",
           #"<=",
           #rep("<=", n),
           "<=",
           "==")
  
  b <- c(nQB,
         nRB,
         nWR,
         nTE,
         nK,
         nDST,
         #maxRisk,
         #rep(maxRisk, n),
         budget,
         nStarters)
  
  sol <- Rglpk_solve_LP(obj = pool$Points, mat = A, dir = dir, rhs = b, types = var.types, max = T)
  sol <- as.data.frame(pool[sol$solution == 1, c("Name", "Team", "Position", "Points", "Salary")])
  rownames(sol) <- NULL
 return(sol)
}

sol <- optimizeTeam(pool, nQB, nRB, nWR, nTE, nK, nDST, budget, nStarters)