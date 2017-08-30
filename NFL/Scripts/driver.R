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
budget <- 60000
nStarters <- 9

# list of data sources
sources <- c("cbs", "espn", "fantasySharks", "fftoday", "fleaflicker", "fox", "nfl", "numberFire",
             "rotoviz", "yahoo")
# download and save projections from all data sources
# for (i in 1:length(sources)) {
#   source(paste0("./Scripts/", sources[i], ".R"))
# }
# load data frames
for (i in 1:length(sources)) {
  load(paste0("./Data/Week ", week, "/", sources[i], ".RData"))
}

# players with inconsistent names ("Le'Veon Bell"?)
goodNames <- c("Steve Smith", "Odell Beckham Jr.", "Duke Johnson", "Ted Ginn", "Robert Griffin III",
               "Cecil Shorts", "A.J. Green", "T.Y. Hilton", "C.J. Anderson", "T.J. Yeldon",
               "C.J. Spiller", "LeSean McCoy", "LeGarrette Blount", "DeAndre Hopkins",
               "DeMarco Murray", "DeAngelo Williams")

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
  else if (name == "Lesean Mccoy" | name == "Lesean McCoy") {name <- "LeSean McCoy"}
  else if (name == "Legarrette Blount") {name <- "LeGarrette Blount"}
  else if (name == "Deandre Hopkins") {name <- "DeAndre Hopkins"}
  else if (name == "Demarco Murray") {name <- "DeMarco Murray"}
  else if (name == "Deangelo Williams") {name <- "DeAngelo Williams"}
  else if (name == "Jerick Mckinnon") {name <- "Jerick McKinnon"}
  else if (name == "Darren Mcfadden") {name <- "Darren McFadden"}
  else if (name == "Dexter Mccluster") {name <- "Dexter McCluster"}
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

# FFToday
fftoday <- fftoday[, 1:3]
names(fftoday)[3] <- "FFToday"
fftoday$Player <- sapply(fftoday$Player, replaceName)

# fleaflicker
fleaflicker <- fleaflicker[, 1:3]
names(fleaflicker)[3] <- "fleaflicker"
fleaflicker$Player <- sapply(fleaflicker$Player, replaceName)
fleaflicker$Pos <- sub("D/ST", "DST", fleaflicker$Pos)

# Fox
fox <- fox[, 1:3]
names(fox)[3] <- "Fox"
fox$Player <- sapply(fox$Player, replaceName)

# NFL
nfl <- nfl[, 1:3]
names(nfl)[3] <- "NFL"
nfl$Player <- sapply(nfl$Player, replaceName)

# numberFire
numberFire <- numberFire[, c("Player", "Pos", "FP", "FD_FP", "FD_Cost")]
names(numberFire)[3:5] <- c("numberFire", "FanDuel", "Cost")
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

# join data frames
pool <- full_join(cbs, espn, by = c("Player", "Pos")) %>% full_join(fantasySharks, by = c("Player", "Pos")) %>%
        full_join(fftoday, by = c("Player", "Pos")) %>% full_join(fleaflicker, by = c("Player", "Pos")) %>% 
        full_join(fox, by = c("Player", "Pos")) %>% full_join(nfl, by = c("Player", "Pos")) %>% 
        full_join(numberFire, by = c("Player", "Pos")) %>% full_join(rotoviz, by = c("Player", "Pos")) %>% 
        full_join(yahoo, by = c("Player", "Pos"))

# remove players who don't cost anything
pool <- pool %>% filter(!is.na(Cost)) %>% filter(Cost != 0)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "Cost")],
              pool[, -which(colnames(pool) %in% c("Player", "Pos", "Cost"))])
# average projected points
pool$Avg <- rowMeans(pool[, -(1:3)], na.rm = T)
# standard deviation of projected points
pool$SD <- sapply(data.frame(t(pool[, -(1:3)])), sd, na.rm = T)
# round all numbers to 2 decimal points
pool[, -(1:3)] <- sapply(pool[, -(1:3)], round, digits = 2)
# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "Cost", "Avg", "SD")],
              pool[, -which(colnames(pool) %in% c("Player", "Pos", "Cost", "Avg", "SD"))])
# sort player pool by average projected points
pool <- arrange(pool, desc(Avg))

# optimizer
optimizeTeam <- function(pool, nQB, nRB, nWR, nTE, nK, nDST, budget, nStarters){
  # number of players in the pool
  n <- length(pool$Player)
  # set variable types to "binary" for solver
  var.types <- rep("B", n)
  
  A <- rbind(as.numeric(pool$Pos == "QB"),
             as.numeric(pool$Pos == "RB"),
             as.numeric(pool$Pos == "WR"),
             as.numeric(pool$Pos == "TE"),
             as.numeric(pool$Pos == "K"),
             as.numeric(pool$Pos == "DST"),
             #optimizeData$risk,
             #diag(optimizeData$risk),
             pool$Cost,
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
  
  optimalTeam <- Rglpk_solve_LP(obj = pool$Avg, mat = A, dir = dir, rhs = b, types = var.types, max = T)
  optimalTeam <- as.data.frame(pool[optimalTeam$solution == 1, c("Player", "Pos", "Avg", "Cost")])
  rownames(optimalTeam) <- NULL
  return(optimalTeam)
}

optimalTeam <- optimizeTeam(pool, nQB, nRB, nWR, nTE, nK, nDST, budget, nStarters)