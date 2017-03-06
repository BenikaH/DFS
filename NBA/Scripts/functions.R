replaceName <- function(name) {
  if (name == "Juan Jose Barea" | name == "J.J. Barea" | name == "Jose Barea") {name <- "Jose Juan Barea"}
  else if (name == "Kyle O`Quinn") {name <- "Kyle O'Quinn"}
  else if (name == "Guillermo Hernangomez") {name <- "Willy Hernangomez"}
  else if (name == "Kelly Oubre Jr.") {name <- "Kelly Oubre"}
  else if (name == "Glenn Robinson III") {name <- "Glenn Robinson"}
  else if (name == "Amar Stoudemire") {name <- "Amar'e Stoudemire"}
  else if (name == "Raulzinho Neto") {name <- "Raul Neto"}
  else if (name == "Moe Harkless") {name <- "Maurice Harkless"}
  else if (name == "Lou Williams") {name <- "Louis Williams"}
  else if (name == "Johnny O'Bryant III") {name <- "Johnny O'Bryant"}
  else if (name == "Ish Smith") {name <- "Ishmael Smith"}
  else if (name == "Patty Mills") {name <- "Patrick Mills"}
  else if (name == "Luc Richard Mbah a Moute") {name <- "Luc Mbah a Moute"}
  else if (name == "D`Angelo Russell") {name <- "D'Angelo Russell"}
  else if (name == "Clint N`Dumba-Capela") {name <- "Clint Capela"}
  else if (name == "Brad Beal") {name <- "Bradley Beal"}
  else if (name == "Tim Hardaway") {name <- "Tim Hardaway Jr."}
  else if (name == "Larry Nance Jr.") {name <- "Larry Nance"}
  else if (name == "E`Twaun Moore") {name <- "E'Twaun Moore"}
  else {name}
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

# function to calculate scoring bonuses on DraftKings
bonus <- function(x) {
  if (x$PTS >= 10 & x$TRB >= 10 & (x$AST >= 10 | x$BLK >= 10 | x$STL >= 10)) {
    y <- 4.5
  } else if (x$PTS >= 10 & (x$TRB >= 10 | x$AST >= 10 | x$BLK >= 10 | x$STL >= 10)) {
    y <- 1.5
  } else {
    y <- 0
  }
}

teamAbbr <- function(team) {
  if (team == "Atlanta") {team <- "ATL"}
  else if (team == "Boston") {team <- "BOS"}
  else if (team == "Brooklyn") {team <- "BKN"}
  else if (team == "Charlotte") {team <- "CHA"}
  else if (team == "Chicago") {team <- "CHI"}
  else if (team == "Cleveland") {team <- "CLE"}
  else if (team == "Dallas") {team <- "DAL"}
  else if (team == "Denver") {team <- "DEN"}
  else if (team == "Detroit") {team <- "DET"}
  else if (team == "Golden State") {team <- "GSW"}
  else if (team == "Houston") {team <- "HOU"}
  else if (team == "Indiana") {team <- "IND"}
  else if (team == "L.A. Clippers") {team <- "LAC"}
  else if (team == "L.A. Lakers") {team <- "LAL"}
  else if (team == "Memphis") {team <- "MEM"}
  else if (team == "Miami") {team <- "MIA"}
  else if (team == "Milwaukee") {team <- "MIL"}
  else if (team == "Minnesota") {team <- "MIN"}
  else if (team == "New Orleans") {team <- "NOR"}
  else if (team == "New York") {team <- "NYK"}
  else if (team == "Oklahoma City") {team <- "OKC"}
  else if (team == "Orlando") {team <- "ORL"}
  else if (team == "Philadelphia") {team <- "PHI"}
  else if (team == "Phoenix") {team <- "PHO"}
  else if (team == "Portland") {team <- "POR"}
  else if (team == "Sacramento") {team <- "SAC"}
  else if (team == "San Antonio") {team <- "SAS"}
  else if (team == "Toronto") {team <- "TOR"}
  else if (team == "Utah") {team <- "UTH"}
  else if (team == "Washington") {team <- "WAS"}
}