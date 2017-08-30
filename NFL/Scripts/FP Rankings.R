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
nK <- 0
nDST <- 1
budget <- 200
nStarters <- 9 

# download rankings from FantasyPros
qb <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/qb.php", stringsAsFactors = FALSE)$data
rb <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/rb.php", stringsAsFactors = FALSE)$data
wr <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/wr.php", stringsAsFactors = FALSE)$data
te <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/te.php", stringsAsFactors = FALSE)$data
k <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/k.php", stringsAsFactors = FALSE)$data
dst <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/dst.php", stringsAsFactors = FALSE)$data

# rename second column in DST table
names(dst)[2] <- "Player"

# add column for player position
qb$Position <- as.factor("QB")
rb$Position <- as.factor("RB")
wr$Position <- as.factor("WR")
te$Position <- as.factor("TE")
k$Position <- as.factor("K")
dst$Position <- as.factor("DST")

# merge players across positions
rankings <- rbind(qb, rb, wr, te, k, dst)
# get rid of blank rows
rankings <- rankings %>% na.omit()

# player name
rankings$Name <- sub("\\s[A-Z]{2,}.*", "", rankings$Player)
# rankings$Name <- sub("\\s[A-Z]$", "", rankings$Name)
# first name
rankings$First <- sapply(strsplit(rankings$Name, " "), '[[', 1)
# last name
rankings$Last <- sapply(strsplit(rankings$Name, " "), '[[', 2)
# team name
rankings$Team <- str_extract(rankings$Player, "[A-Z]{2,}")

# retain a subset of columns
rankings <- rankings[c("Name", "First", "Last", "Team", "Position", "Avg", "Std Dev", "Best", "Worst")]
# change class of numeric columns
rankings[, c("Avg", "Std Dev", "Best", "Worst")] <- sapply(rankings[, c("Avg", "Std Dev", "Best", "Worst")], as.numeric)

# download Yahoo DFS player pool
pool <- readHTMLTable("http://www.fantasypros.com/nfl/yahoo-lineup-optimizer.php", stringsAsFactors = FALSE)[["player-pool"]]
# retain a subset of columns
pool <- pool[, c(1:2, 5:6)]
# set column names
names(pool) <- c("Player", "PosRank", "Points", "Salary")
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

# retain a subset of columns
pool <- pool[c("Name", "First", "Last", "Team", "Position", "Rank", "Points", "Salary")]
# set position as factor
pool$Position <- as.factor(pool$Position)
# change class of numeric columns
pool[, c("Rank", "Points", "Salary")] <- sapply(pool[, c("Rank", "Points", "Salary")], as.numeric)
# sort player pool by projected points
pool <- pool %>% arrange(desc(Points))

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
           #"<=",
           #rep("<=", n),
           "<=",
           "==")
  
  b <- c(nQB,
         nRB,
         nWR,
         nTE,
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