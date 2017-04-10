# function for reading FanGraphs tables by position
readFanGraphs <- function(pos) {
  # set URL depending on position
  if (pos == "P") {
    URL <- "http://www.fangraphs.com/dailyprojections.aspx?pos=all&stats=pit&type=sabersim&team=0&lg=all&players=0"
  } else {
    URL <- paste0("http://www.fangraphs.com/dailyprojections.aspx?pos=", tolower(pos),
                  "&stats=bat&type=sabersim&team=0&lg=all&players=0")
  }
  # read table
  pool <- read_html(URL) %>% html_node('table[class="rgMasterTable"]') %>% html_table(header = F)
  # remove extraneous rows
  pool <- pool[!grepl("pages$", pool[, 1]), ]
  # set column names
  names(pool) <- pool[1, ]
  pool <- pool[-1, ]
  # select relevant columns and add position column
  pool <- pool %>% select(Name, FanDuel, DraftKings) %>% mutate(Pos = pos)
}

# positions
pos <- c("P", "C", "1B", "2B", "SS", "3B", "RF", "CF", "LF", "DH")

# create player pool
pool <- bind_rows(lapply(pos, readFanGraphs))

# rearrange columns
pool <- pool[, c("Name", "Pos", "FanDuel", "DraftKings")]
# rename columns
names(pool)[1] <- "Player"
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)
# convert projections to numeric
pool[, c("FanDuel", "DraftKings")] <- sapply(pool[, c("FanDuel", "DraftKings")], as.numeric)

# save FD file
fangraphs <- pool[, c("Player", "FanDuel")]
names(fangraphs)[2] <- "FanGraphs"
save(fangraphs, file = "Data/FD/FanGraphs.RData")
# save DK file
fangraphs <- pool[, c("Player", "DraftKings")]
names(fangraphs)[2] <- "FanGraphs"
save(fangraphs, file = "Data/DK/FanGraphs.RData")