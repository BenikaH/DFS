# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library("dplyr")
library("stringr")

# set week number
week <- 6

# load exposure
load(paste0("./Data/Week ", week, "/exposure.RData"))

# expected <- sum(exposure$Med[exposure$Kickoff != "Sun 8:30PM" & exposure$Kickoff != "Mon 8:30PM"] *
#                 exposure$Exposure[exposure$Kickoff != "Sun 8:30PM" & exposure$Kickoff != "Mon 8:30PM"])

expected <- sum(exposure$Med[exposure$Kickoff != "Mon 8:30PM"] *
                  exposure$Exposure[exposure$Kickoff != "Mon 8:30PM"])

total <- sum(exposure$Med * exposure$Exposure)