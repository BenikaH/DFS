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

# load actual results
load(paste0("./Data/Week ", week, "/actual.RData"))
# load best lineups
load(paste0("./Data/Week ", week, "/bestLineups.RData"))

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

# actual results
actual <- actual[, 1:3]
names(actual)[3] <- "Pts"
actual$Player <- sapply(actual$Player, replaceName)

# calculate total points scored by each team
lineup <- as.character(t(bestLineups[1, ]))
total <- sum(actual$Pts[actual$Player %in% lineup])
for (i in 2:nrow(bestLineups)) {
  lineup <- as.character(t(bestLineups[i, ]))
  total[i] <- sum(actual$Pts[actual$Player %in% lineup])
}

# histogram of team point totals
plot(density(total))