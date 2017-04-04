# download projections from Rotowire
x <- paste0("http://www.rotowire.com/daily/MLB/optimizer.php?site=", site, "&sport=MLB")
pool <- readHTMLTable(x, stringsAsFactors = F)[["playerPoolTable"]][, 2:14]

# set column names
names(pool) <- c("Player", "Team", "Pos", "Order", "Opp", "Salary", "Rotowire", "Value", "Line", "OU",
                 "TmRuns", "Weather", "Pitcher")
# get player salaries
pool$Salary <- html_nodes(read_html(x), ".salaryInput") %>% html_attr("value")
# convert salaries to numeric
pool$Salary <- as.numeric(gsub("[\\$,]", "", pool$Salary))
# get player projections
pool$Rotowire <- as.numeric(html_nodes(read_html(x), ".ptsInput") %>% html_attr("value"))
# clean up pitcher names
pool$Pitcher <- sub("\\.", "", pool$Pitcher)
# clean up player names
pool$Player <- sub("\\s+\\(.*\\)$", "", pool$Player)
# covert columns to numeric
pool[, c("Order", "Value", "Line", "OU", "TmRuns")] <- sapply(pool[, c("Order", "Value", "Line", "OU", "TmRuns")], 
                                                              as.numeric)

# retain relevant columns
pool <- pool[, c("Player", "Rotowire")]
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)

# save file
rotowire <- pool
save(rotowire, file = paste0("Data/", sFlag, "/Rotowire.RData"), row.names = F)