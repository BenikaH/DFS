# function to standardize inconsistent player names
replaceName <- function(name) {
  if (name == "Steve Smith Sr.") {name <- "Steve Smith"}
  else if (name == "Terrelle Pryor Sr.") {name <- "Terrelle Pryor"}
  else if (name == "Odell Beckham") {name <- "Odell Beckham Jr."}
  else if (name == "Duke Johnson Jr.") {name <- "Duke Johnson"}
  else if (name == "Ted Ginn Jr.") {name <- "Ted Ginn"}
  else if (name == "Marvin Jones Jr.") {name <- "Marvin Jones"}
  else if (name == "Robert Griffin") {name <- "Robert Griffin III"}
  else if (name == "Cecil Shorts III") {name <- "Cecil Shorts"}
  else if (name == "Aj Green") {name <- "A.J. Green"}
  else if (name == "Ty Hilton") {name <- "T.Y. Hilton"}
  else if (name == "Cj Anderson") {name <- "C.J. Anderson"}
  else if (name == "Tj Yeldon") {name <- "T.J. Yeldon"}
  else if (name == "Cj Spiller") {name <- "C.J. Spiller"}
  else if (name == "Ej Manuel" | name == "EJ Manuel") {name <- "E.J. Manuel"}
  else if (name == "Leveon Bell") {name <- "Le'Veon Bell"}
  else if (name == "Lesean Mccoy" | name == "Lesean McCoy") {name <- "LeSean McCoy"}
  else if (name == "Desean Jackson") {name <- "DeSean Jackson"}
  else if (name == "Legarrette Blount") {name <- "LeGarrette Blount"}
  else if (name == "Deandre Hopkins") {name <- "DeAndre Hopkins"}
  else if (name == "Demarco Murray") {name <- "DeMarco Murray"}
  else if (name == "Deangelo Williams") {name <- "DeAngelo Williams"}
  else if (name == "Jerick Mckinnon") {name <- "Jerick McKinnon"}
  else if (name == "Darren Mcfadden") {name <- "Darren McFadden"}
  else if (name == "Dexter Mccluster") {name <- "Dexter McCluster"}
  else if (name == "DeVante Parker") {name <- "Devante Parker"}
  else if (name == "Josh Mccown") {name <- "Josh McCown"}
  else if (name == "Luke Mccown") {name <- "Luke McCown"}
  else if (name == "Brandon Mcmanus") {name <- "Brandon McManus"}
  else if (name == "Mike Vick") {name <- "Michael Vick"}
  else if (name == "Stevie Johnson") {name <- "Steve Johnson"}
  else if (name == "Boobie Dixon") {name <- "Anthony Dixon"}
  else if (name == "Chris Ivory") {name <- "Christopher Ivory"}
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
  else if (name == "Los Angeles" | name == "Rams") {name <- "Los Angeles Rams"}
  else if (name == "Tampa Bay" | name == "Buccaneers") {name <- "Tampa Bay Buccaneers"}
  else if (name == "Tennessee" | name == "Titans") {name <- "Tennessee Titans"}
  else if (name == "Washington" | name == "Redskins") {name <- "Washington Redskins"}
  else {name}
}

# function to capitalize just the first letter of each name
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# function to calculate standard error from sample of means
calcSE <- function(x) {
  n <- length(x[!is.na(x)])
  if (n > 1) {
    y <- sd(x, na.rm = T)
  } else {
    y <- 0
  }
}

# function to calculate optimal lineup
optimizeTeam <- function(pts, playerInfo, constraints) {
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
  optimalTeam <- as.data.frame(cbind(playerInfo, solList$solution))
  names(optimalTeam)[ncol(playerInfo) + 1] <- "Binary"
  rownames(optimalTeam) <- NULL
  return(optimalTeam)
}

fmincon <- function(pts, playerInfo, constraints) {
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
  
  knapsack <- function(x, pts, playerInfo, constraints) {
    f <- x %*% pts
#     A <- rbind(as.numeric(playerInfo$Pos == "QB"),
#                as.numeric(playerInfo$Pos == "RB"),
#                as.numeric(playerInfo$Pos == "WR"),
#                as.numeric(playerInfo$Pos == "TE"),
#                as.numeric(playerInfo$Pos == "K"),
#                as.numeric(playerInfo$Pos == "DST"),
#                playerInfo$Cost)
#     b <- constraints[c(1:6, 8)]
    # penalty <- abs(sum((A %*% x - b) / b))
    penalty <- abs(x %*% playerInfo$Cost - constraints[8])
    return(f - penalty)
  }
  
  GA <- ga(type = "binary", fitness = knapsack, pts = pts, playerInfo = playerInfo,
           constraints = constraints, nBits = n, maxiter = 200)
  optimalTeam <- as.data.frame(cbind(playerInfo, as.numeric(GA@solution)))
  names(optimalTeam)[ncol(playerInfo) + 1] <- "Binary"
  rownames(optimalTeam) <- NULL
  return(optimalTeam)
}