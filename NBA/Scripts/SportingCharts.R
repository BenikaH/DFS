# download projections from SportingCharts
url <- getURL("https://www.sportingcharts.com/nba/dfs-projections/")
json <- str_extract(url, 'NBALinupBuilder.*')
json <- sub("^NBALinupBuilder\\(", "", json)
json <- sub("\\);$", "", json)
jsonList <- fromJSON(json)

# convert JSON to data frame
pool <- data.frame(as.character(sapply(jsonList, function(x) x$Name),
                                stringsAsFactors = F),
                   as.character(sapply(jsonList, function(x) x$ProjectedFDPoints),
                                stringsAsFactors = F),
                   stringsAsFactors = F)

# set column names
names(pool) <- c("Player", "SportingCharts")
# set numeric columns
pool$SportingCharts <- as.numeric(pool$SportingCharts)
pool$SportingCharts <- round(pool$SportingCharts, 2)
# standardize player names
pool$Player <- sapply(pool$Player, replaceName)

# save file
sportingcharts <- pool
save(sportingcharts, file = paste0("Data/", sFlag, "/SportingCharts.RData"), row.names = F)