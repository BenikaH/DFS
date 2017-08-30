# scoring system
if (sFlag == "FD") {
  scoring <- data.frame(t(c(0.04, 4, -1, 0.1, 6, -2, 0.5,
                            0.1, 6, 2, 3, 1, 2, 2,
                            1, 6, 2)))
} else if (sFlag == "DK") {  
  scoring <- data.frame(t(c(0.04, 4, -1, 0.1, 6, -1, 1,
                            0.1, 6, 2, 3, 1, 2, 2,
                            1, 6, 2)))
}
names(scoring) <- c("passYd", "passTD", "Int", "rushYd", "rushTD", "Fum", "Rec",
                    "recYd", "recTD", "2PT", "FG", "XP", "DefInt", "FumRec",
                    "Sack", "DefTD", "Safety")

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