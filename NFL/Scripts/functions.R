# optimizer
optimizeTeam <- function(pool, nQB, nRB, nWR, nTE, nK, nDEF, budget) {
  # number of players in the pool
  n <- length(pool$Player)
  # set variable types to "binary" for solver
  var.types <- rep("B", n)
  
  A <- rbind(as.numeric(pool$Pos == "QB"),
             as.numeric(pool$Pos == "RB"),
             as.numeric(pool$Pos == "WR"),
             as.numeric(pool$Pos == "TE"),
             as.numeric(pool$Pos == "K"),
             as.numeric(pool$Pos == "DEF"),
             #optimizeData$risk,
             #diag(optimizeData$risk),
             round(pool$Cost))   
  
  dir <- c("==",
           "==",
           "==",
           "==",
           "==",
           "==",
           #"<=",
           #rep("<=", n),
           "<=")
  
  b <- c(nQB,
         nRB,
         nWR,
         nTE,
         nK,
         nDEF,
         #maxRisk,
         #rep(maxRisk, n),
         budget)
  
  optimalTeam <- Rglpk_solve_LP(obj = pool$Points, mat = A, dir = dir, rhs = b, types = var.types, max = T)
  optimalTeam <- as.data.frame(pool[optimalTeam$solution == 1, c("Player", "Pos", "Team", "Cost", "Points")])
  rownames(optimalTeam) <- NULL
  return(optimalTeam)
}

# function to standardize inconsistent player names
replaceName <- function(name) {
  if (name == "Steve Smith") {name <- "Steve Smith Sr."}
  else if (name == "Odell Beckham") {name <- "Odell Beckham Jr."}
  else if (name == "Duke Johnson") {name <- "Duke Johnson Jr."}
  else if (name == "Marvin Jones") {name <- "Marvin Jones Jr."}
  else if (name == "Ted Ginn") {name <- "Ted Ginn Jr."}
  else if (name == "Robert Griffin") {name <- "Robert Griffin III"}
  else if (name == "LeVeon Bell") {name <- "Le'Veon Bell"}
  else if (name == "Cardinals") {name <- "Arizona"}
  else if (name == "Falcons") {name <- "Atlanta"}
  else if (name == "Ravens") {name <- "Baltimore"}
  else if (name == "Bills") {name <- "Buffalo"}
  else if (name == "Panthers") {name <- "Carolina"}
  else if (name == "Bears") {name <- "Chicago"}
  else if (name == "Bengals") {name <- "Cincinnati"}
  else if (name == "Browns") {name <- "Cleveland"}
  else if (name == "Cowboys") {name <- "Dallas"}
  else if (name == "Broncos") {name <- "Denver"}
  else if (name == "Lions") {name <- "Detroit"}
  else if (name == "Packers") {name <- "Green Bay"}
  else if (name == "Texans") {name <- "Houston"}
  else if (name == "Colts") {name <- "Indianapolis"}
  else if (name == "Jaguars") {name <- "Jacksonville"}
  else if (name == "Chiefs") {name <- "Kansas City"}
  else if (name == "Rams") {name <- "Los Angeles"}
  else if (name == "Dolphins") {name <- "Miami"}
  else if (name == "Vikings") {name <- "Minnesota"}
  else if (name == "Patriots") {name <- "New England"}
  else if (name == "Saints") {name <- "New Orleans"}
  else if (name == "Giants") {name <- "New York Giants"}
  else if (name == "Jets") {name <- "New York Jets"}
  else if (name == "Raiders") {name <- "Oakland"}
  else if (name == "Eagles") {name <- "Philadelphia"}
  else if (name == "Steelers") {name <- "Pittsburgh"}
  else if (name == "Chargers") {name <- "San Diego"}
  else if (name == "49ers") {name <- "San Francisco"}
  else if (name == "Seahawks") {name <- "Seattle"}
  else if (name == "Buccaneers") {name <- "Tampa Bay"}
  else if (name == "Titans") {name <- "Tennessee"}
  else if (name == "Redskins") {name <- "Washington"}
  else {name}
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}