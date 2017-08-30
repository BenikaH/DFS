library(XML)
library(rvest)
library(tidyr)

# read AAV table from FFToday
fftoday <- readHTMLTable("http://www.fftoday.com/rankings/17_auction_values.html",
                         stringsAsFactors = F)[["myTable"]][, -1]
# rename AAV column
names(fftoday)[4] <- "AAV"
# remove rookie symbols from player names
fftoday$Player <- sub(" ®", "", fftoday$Player)

# read list of players from Walter Football
wfb <- read_html("http://walterfootball.com/fantasycheatsheet/2017/Traditional") %>%
  html_nodes("li") %>% html_text()
# remove trailing whitespace and plus signs from text
wfb <- gsub("\\+", "", trimws(wfb))
# use " Bye: " string to identify li elements containing players and their AAV values
wfb <- grep(" Bye: ", wfb, value = T)
# remove bye weeks
wfb <- gsub(" Bye: [0-9]{1,2}", "", wfb)
# split character vector into data frame
wfb <- data.frame(do.call(rbind, strsplit(wfb, ", ")), stringsAsFactors = F)
names(wfb) <- c("Player", "Pos", "Team")
wfb <- separate(wfb, Team, c("Team", "AAV"), "\\. ")
# convert AAV to numeric
wfb$AAV <- as.numeric(sub("\\$", "", wfb$AAV))