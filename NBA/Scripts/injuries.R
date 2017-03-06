injuries <- readHTMLTable("http://www.donbest.com/nba/injuries/", stringsAsFactors = F)[[2]]

# omit rows with NA
injuries <- na.omit(injuries)
# set column names
names(injuries) <- c("Date", "Pos", "Player", "Injury", "Status")
# remove header rows
injuries <- filter(injuries, Player != "Player")
# standardize player names
injuries$Player <- sapply(injuries$Player, replaceName)
# remove players who are listed as probable
injuries <- injuries[!grepl("probable", injuries$Status), ]