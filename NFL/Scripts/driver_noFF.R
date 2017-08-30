# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("XML")
library("dplyr")
library("stringr")
library("readxl")
library("Rglpk")

# set week number
week <- 3

# players to exclude
exclude <- ""

# scoring system
scoring <- data.frame(t(c(0.04, 4, -1, 0.1, 6, -2, 0.5, 0.1, 6, 2, 3, 1, 2, 2, 1, 6, 2)))
names(scoring) <- c("passYd", "passTD", "Int", "rushYd", "rushTD", "Fum", "Rec", "recYd", "recTD", "2PT",
                    "FG", "XP", "DefInt", "FumRec", "Sack", "DefTD", "Safety")

# league settings
nQB <- 1
nRB <- 2
nWR <- 3
nTE <- 1
nK <- 1
nDST <- 1
nStarters <- 9
budget <- 60000
constraints <- c(nQB, nRB, nWR, nTE, nK, nDST, nStarters, budget)

# list of data sources
sources <- c("cbs", "espn", "fantasySharks", "fox", "numberFire", "rotoviz", "yahoo")
# download and save projections from all data sources
# for (i in 1:length(sources)) {
#   source(paste0("./Scripts/", sources[i], ".R"))
# }
# source("./Scripts/fantasypros.R")
# load data frames
for (i in 1:length(sources)) {
  load(paste0("./Data/Week ", week, "/", sources[i], ".RData"))
}
load(paste0("./Data/Week ", week, "/fantasypros.RData"))

# function for standardizing inconsistent player names
replaceName <- function(name) {
  if (name == "Steve Smith Sr.") {name <- "Steve Smith"}
  else if (name == "Odell Beckham") {name <- "Odell Beckham Jr."}
  else if (name == "Duke Johnson Jr.") {name <- "Duke Johnson"}
  else if (name == "Ted Ginn Jr.") {name <- "Ted Ginn"}
  else if (name == "Robert Griffin") {name <- "Robert Griffin III"}
  else if (name == "Cecil Shorts III") {name <- "Cecil Shorts"}
  else if (name == "Aj Green") {name <- "A.J. Green"}
  else if (name == "Ty Hilton") {name <- "T.Y. Hilton"}
  else if (name == "Cj Anderson") {name <- "C.J. Anderson"}
  else if (name == "Tj Yeldon") {name <- "T.J. Yeldon"}
  else if (name == "Cj Spiller") {name <- "C.J. Spiller"}
  else if (name == "Leveon Bell") {name <- "Le'Veon Bell"}
  else if (name == "Lesean Mccoy" | name == "Lesean McCoy") {name <- "LeSean McCoy"}
  else if (name == "Legarrette Blount") {name <- "LeGarrette Blount"}
  else if (name == "Deandre Hopkins") {name <- "DeAndre Hopkins"}
  else if (name == "Demarco Murray") {name <- "DeMarco Murray"}
  else if (name == "Deangelo Williams") {name <- "DeAngelo Williams"}
  else if (name == "Jerick Mckinnon") {name <- "Jerick McKinnon"}
  else if (name == "Darren Mcfadden") {name <- "Darren McFadden"}
  else if (name == "Dexter Mccluster") {name <- "Dexter McCluster"}
  else if (name == "Josh Mccown") {name <- "Josh McCown"}
  else if (name == "Luke Mccown") {name <- "Luke McCown"}
  else if (name == "Brandon Mcmanus") {name <- "Brandon McManus"}
  else if (name == "Arizona" | name == "Cardinals") {name <- "Arizona Cardinals"}
  else if (name == "Atlanta" | name == "Falcons") {name <- "Atlanta Falcons"}
  else if (name == "Baltimore" | name == "Ravens") {name <- "Baltimore Ravens"}
  else if (name == "Buffalo" | name == "Bills") {name <- "Buffalo Bills"}
  else if (name == "Carolina" | name == "Panthers") {name <- "Carolina Panthers"}
  else if (name == "Chicago" | name == "Bears") {name <- "Chicago Bears"}
  else if (name == "Cincinnati" | name == "Bengals") {name <- "Cincinnati Bengals"}
  else if (name == "Cleveland" | name == "Browns") {name <- "Cleveland Browns"}
  else if (name == "Dallas" | name == "Cowboys") {name <- "Dallas Cowboys"}
  else if (name == "Denver" | name == "Broncos") {name <- "Denver Broncos"}
  else if (name == "Detroit" | name == "Lions") {name <- "Detroit Lions"}
  else if (name == "Green Bay" | name == "Packers") {name <- "Green Bay Packers"}
  else if (name == "Houston" | name == "Texans") {name <- "Houston Texans"}
  else if (name == "Indianapolis" | name == "Colts") {name <- "Indianapolis Colts"}
  else if (name == "Jacksonville" | name == "Jaguars") {name <- "Jacksonville Jaguars"}
  else if (name == "Kansas City" | name == "Chiefs") {name <- "Kansas City Chiefs"}
  else if (name == "Miami" | name == "Dolphins") {name <- "Miami Dolphins"}
  else if (name == "Minnesota" | name == "Vikings") {name <- "Minnesota Vikings"}
  else if (name == "New England" | name == "Patriots") {name <- "New England Patriots"}
  else if (name == "New Orleans" | name == "Saints") {name <- "New Orleans Saints"}
  else if (name == "Giants") {name <- "New York Giants"}
  else if (name == "Jets") {name <- "New York Jets"}
  else if (name == "Oakland" | name == "Raiders") {name <- "Oakland Raiders"}
  else if (name == "Philadelphia" | name == "Eagles") {name <- "Philadelphia Eagles"}
  else if (name == "Pittsburgh" | name == "Steelers") {name <- "Pittsburgh Steelers"}
  else if (name == "San Diego" | name == "Chargers") {name <- "San Diego Chargers"}
  else if (name == "San Francisco" | name == "49ers") {name <- "San Francisco 49ers"}
  else if (name == "Seattle" | name == "Seahawks") {name <- "Seattle Seahawks"}
  else if (name == "St. Louis" | name == "Rams") {name <- "St. Louis Rams"}
  else if (name == "Tampa Bay" | name == "Buccaneers") {name <- "Tampa Bay Buccaneers"}
  else if (name == "Tennessee" | name == "Titans") {name <- "Tennessee Titans"}
  else if (name == "Washington" | name == "Redskins") {name <- "Washington Redskins"}
  else {name}
}

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

# rotoviz
rotoviz <- rotoviz[, 1:3]
names(rotoviz)[3] <- "rotoviz"
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
rotoviz$Player <- sapply(tolower(rotoviz$Player), simpleCap)
rotoviz$Player <- sapply(rotoviz$Player, replaceName)

# Yahoo
yahoo <- yahoo[, 1:3]
names(yahoo)[3] <- "Yahoo"
yahoo$Player <- sapply(yahoo$Player, replaceName)

# FantasyPros
fantasypros$Player <- sapply(fantasypros$Player, replaceName)
# FanDuel DST projection are extremely high, so set them to NA
fantasypros[fantasypros$Pos == "DST", "FanDuel"] <- NA

# join data frames
pool <- full_join(cbs, espn, by = c("Player", "Pos")) %>% full_join(fantasySharks, by = c("Player", "Pos")) %>%
        full_join(fox, by = c("Player", "Pos")) %>% full_join(numberFire, by = c("Player", "Pos")) %>%
        full_join(rotoviz, by = c("Player", "Pos")) %>% full_join(yahoo, by = c("Player", "Pos")) %>%
        full_join(fantasypros, by = c("Player", "Pos"))

# function for calculating population standard deviation from sample of means
calcSD <- function(x) {
  n <- length(x[!is.na(x)])
  c4 <- sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2)
  # y <- sd(x, na.rm = T) * sqrt(n) / c4
  if (n > 1) {
    y <- mad(x, na.rm = T) * sqrt(n) / c4
  } else {
    y <- 0
  }
}

# remove players from the exclude list
pool <- pool %>% filter(!(Player %in% exclude))
# remove players who don't cost anything
pool <- pool %>% filter(!is.na(Cost)) %>% filter(Cost != 0)
# remove players from games prior to Sun 1:00PM
pool <- pool %>% filter(Kickoff != "Thu 8:25PM" & Kickoff != "Sun 9:30AM")
# rearrange columns
pool <- cbind(pool[, c("Player", "Team", "Pos", "Kickoff", "Cost")],
              pool[, -which(colnames(pool) %in% c("Player", "Team", "Pos", "Kickoff", "Cost"))])
# average projected points
# poolAvg <- rowMeans(pool[, -(1:5)], na.rm = T)
poolAvg <- sapply(data.frame(t(pool[, -(1:5)])), median, na.rm = T)
# standard deviation of projected points
poolSD <- sapply(data.frame(t(pool[, -(1:5)])), calcSD)
# adjust standard deviations so that players can't score less than zero
poolSD[poolSD > poolAvg / 3] <- poolAvg[poolSD > poolAvg / 3] / 3
# store Avg and SD in player pool
pool$Avg <- poolAvg
pool$SD <- poolSD
# remove players with Avg == NA
pool <- pool %>% filter(!is.na(Avg))
# round all numbers to 2 decimal points
pool[, -(1:5)] <- sapply(pool[, -(1:5)], round, digits = 2)
# rearrange columns
pool <- cbind(pool[, c("Player", "Team", "Pos", "Kickoff", "Cost", "Avg", "SD")],
              pool[, -which(colnames(pool) %in% c("Player", "Team", "Pos", "Kickoff", "Cost", "Avg", "SD"))])
# sort player pool by average projected points
pool <- arrange(pool, desc(Avg))

# save player pool
save(pool, file = paste0("./Data/Week ", week, "/playerPool.RData"))
write.csv(pool, file = paste0("./Data/Week ", week, "/playerPool.csv"), row.names = F)

# optimizer
optimizeTeam <- function(pts, playerInfo, constraints){
  # QB constraint
  nQB <- constraints[1]
  # RB constraint
  nRB <- constraints[2]
  # WR constraint
  nWR <- constraints[3]
  # TE constraint
  nTE <- constraints[4]
  # K constraint
  nK <- constraints[5]
  # DST constraint
  nDST <- constraints[6]
  # starter constraint
  nStarters <- constraints[7]
  # budget constraint
  budget <- constraints[8]
  
  # number of players in the pool
  n <- length(pts)
  # set variable types to "binary" for solver
  var.types <- rep("B", n)
  
  A <- rbind(as.numeric(playerInfo$Pos == "QB"),
             as.numeric(playerInfo$Pos == "RB"),
             as.numeric(playerInfo$Pos == "WR"),
             as.numeric(playerInfo$Pos == "TE"),
             as.numeric(playerInfo$Pos == "K"),
             as.numeric(playerInfo$Pos == "DST"),
             rep(1, n),
             playerInfo$Cost)
             #optimizeData$risk,
             #diag(optimizeData$risk))   
  
  dir <- c("==",
           ">=",
           ">=",
           ">=",
           "==",
           "==",
           "==",
           "<=")
           #"<=",
           #rep("<=", n))
  
  b <- c(nQB,
         nRB,
         nWR,
         nTE,
         nK,
         nDST,
         nStarters,
         budget)
         #maxRisk,
         #rep(maxRisk, n))
  
  solList <- Rglpk_solve_LP(obj = pts, mat = A, dir = dir, rhs = b, types = var.types, max = T)
  optimalTeam <- as.data.frame(cbind(playerInfo, pts, solList$solution))
  names(optimalTeam)[6:7] <- c("Points", "Binary")
  rownames(optimalTeam) <- NULL
  return(optimalTeam)
}

# most probable optimal team
mostProbable <- optimizeTeam(pool$Avg, pool[, 1:5], constraints)
mostProbable <- mostProbable %>% filter(Binary == 1) %>% select(-Binary)

# save most probable optimal team
save(mostProbable, file = paste0("./Data/Week ", week, "/mostProbable.RData"))
write.csv(mostProbable, file = paste0("./Data/Week ", week, "/mostProbable.csv"), row.names = F)

# number of simulations
nSim <- 1000
# simulated player points
simPoints <- data.frame(t(mapply(rnorm, n = nSim, mean = pool$Avg, sd = pool$SD)))
# simulated optimal teams
simTeams <- lapply(simPoints, optimizeTeam, playerInfo = pool[, 1:5], constraints = constraints)
# tabulate which players are used in each simulation
simBinary <- simTeams[[1]]$Binary
for (i in 2:nSim) {
  simBinary <- cbind(simBinary, simTeams[[i]]$Binary)
}
# points scored by players on optimal teams
pts <- sapply(simPoints * simBinary, sum)
# point total cutoff for best lineups
cutoff <- quantile(pts, 0.95)
# which teams exceed the cutoff
best <- which(pts > cutoff)
# calculate player exposure on the best lineups
exposure <- cbind(pool[, 1:5], round(rowSums(simBinary[, best]) / length(best), 2))
names(exposure)[6] <- "Exposure"
exposure <- exposure %>% arrange(desc(Exposure))

# save player exposure
save(exposure, file = paste0("./Data/Week ", week, "/exposure.RData"))
write.csv(exposure, file = paste0("./Data/Week ", week, "/exposure.csv"), row.names = F)

# format list of best lineups to match FanDuel standard
# lineup ordered as it appears on FanDuel (QB, RB, RB, WR, WR, WR, TE, K, DST)
playerPos <- pool[simBinary[, best[1]] == 1, c("Player", "Pos")]
QB <- playerPos$Player[playerPos$Pos == "QB"]
RB <- playerPos$Player[playerPos$Pos == "RB"]
WR <- playerPos$Player[playerPos$Pos == "WR"]
TE <- playerPos$Player[playerPos$Pos == "TE"]
K <- playerPos$Player[playerPos$Pos == "K"]
DST <- playerPos$Player[playerPos$Pos == "DST"]
bestLineups <- c(QB, RB, WR, TE, K, DST)
for (i in 2:length(best)) {
  playerPos <- pool[simBinary[, best[i]] == 1, c("Player", "Pos")]
  QB <- playerPos$Player[playerPos$Pos == "QB"]
  RB <- playerPos$Player[playerPos$Pos == "RB"]
  WR <- playerPos$Player[playerPos$Pos == "WR"]
  TE <- playerPos$Player[playerPos$Pos == "TE"]
  K <- playerPos$Player[playerPos$Pos == "K"]
  DST <- playerPos$Player[playerPos$Pos == "DST"]
  bestLineups <- rbind(bestLineups, c(QB, RB, WR, TE, K, DST))
}
bestLineups <- data.frame(bestLineups)
names(bestLineups) <- NULL

# save list of best lineups
save(bestLineups, file = paste0("./Data/Week ", week, "/bestLineups.RData"))
write.csv(bestLineups, file = paste0("./Data/Week ", week, "/bestLineups.csv"), row.names = F)

QB <- filter(exposure, Pos == "QB")
RB <- filter(exposure, Pos == "RB")
WR <- filter(exposure, Pos == "WR")
TE <- filter(exposure, Pos == "TE")
K <- filter(exposure, Pos == "K")
DST <- filter(exposure, Pos == "DST")