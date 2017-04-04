replaceName <- function(name) {
  if (name == "Jonathan Gray") {name <- "Jon Gray"}
  else if (name == "Khristopher Davis") {name <- "Khris Davis"}
  else if (name == "JT Realmuto" | name == "Jacob Realmuto") {name <- "J.T. Realmuto"}
  else if (name == "Matthew Joyce") {name <- "Matt Joyce"}
  else if (name == "Jackie Bradley Jr.") {name <- "Jackie Bradley"}
  else if (name == "Tim Anderson") {name <- "Timothy Anderson"}
  else if (name == "Delino DeShields Jr." | name == "Delino Deshields Jr.") {name <- "Delino DeShields"}
  else if (name == "Hyun-soo Kim" | name == "Hyun Soo Kim") {name <- "Hyun-Soo Kim"}
  else if (name == "Nori Aoki") {name <- "Norichika Aoki"}
  else if (name == "Jacoby Jones") {name <- "JaCoby Jones"}
  else if (name == "Nicholas Castellanos") {name <- "Nick Castellanos"}
  else if (name == "Shin-soo Choo") {name <- "Shin-Soo Choo"}
  else if (name == "Yuli Gurriel") {name <- "Yulieski Gurriel"}
  else if (name == "Raul Adalberto Mondesi") {name <- "Raul Mondesi"}
  else if (name == "Jacob Lamb") {name <- "Jake Lamb"}
  else if (name == "Greg Bird") {name <- "Gregory Bird"}
  else if (name == "Steven Souza" | name == "Steven Souza Jr.") {name <- "Steve Souza"}
  else if (name == "Rickie Weeks Jr.") {name <- "Rickie Weeks"}
  else if (name == "Lance McCullers Jr.") {name <- "Lance McCullers"}
  else if (name == "Dan Robertson") {name <- "Daniel Robertson"}
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