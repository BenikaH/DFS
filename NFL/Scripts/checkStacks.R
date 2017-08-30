# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("dplyr")
library("stringr")

# set week number
week <- 5

# load list of best lineups
load(paste0("./Data/Week ", week, "/bestLineups.RData"))

names(bestLineups) <- c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "K", "DST")

stack1 <- filter(bestLineups, QB == "Michael Vick" & (RB1 == "Le'Veon Bell" | RB2 == "Le'Veon Bell"))
exp1 <- nrow(stack1) / nrow(bestLineups)

stack2 <- filter(bestLineups, QB == "Alex Smith" & (RB1 == "Jamaal Charles" | RB2 == "Jamaal Charles"))
exp2 <- nrow(stack2) / nrow(bestLineups)

stack3 <- filter(bestLineups, QB == "Alex Smith" & (WR1 == "Jeremy Maclin" | WR2 == "Jeremy Maclin" |
                                                    WR3 == "Jeremy Maclin"))
exp3 <- nrow(stack3) / nrow(bestLineups)

stack4 <- filter(bestLineups, QB == "Alex Smith" & (RB1 == "Jamaal Charles" | RB2 == "Jamaal Charles") &
                                                   (WR1 == "Jeremy Maclin" | WR2 == "Jeremy Maclin" |
                                                    WR3 == "Jeremy Maclin"))
exp4 <- nrow(stack4) / nrow(bestLineups)

stack5 <- filter(bestLineups, (RB1 == "Jamaal Charles" | RB2 == "Jamaal Charles") &
                              (WR1 == "Jeremy Maclin" | WR2 == "Jeremy Maclin" | WR3 == "Jeremy Maclin"))
exp5 <- nrow(stack5) / nrow(bestLineups)

stack6 <- filter(bestLineups, QB == "Philip Rivers" & (WR1 == "Keenan Allen" | WR2 == "Keenan Allen" |
                                                      WR3 == "Keenan Allen"))
exp6 <- nrow(stack6) / nrow(bestLineups)