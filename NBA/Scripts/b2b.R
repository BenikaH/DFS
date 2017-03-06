# read table from Rotogrinders back-to-back tool
url <- getURL("https://rotogrinders.com/pages/nba-back-to-back-tool-518716")
b2b <- readHTMLTable(url)[[1]][, c(1, 3)]

# rename columns
names(b2b) <- c("Team", "Situation")
# coerce team names as character
b2b$Team <- as.character(b2b$Team)
# replace team city with team abbreviation
b2b$Team <- sapply(b2b$Team, teamAbbr)