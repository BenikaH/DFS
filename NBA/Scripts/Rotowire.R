# download projections from Rotowire
x <- paste0("http://www.rotowire.com/daily/NBA/optimizer.php?site=", site, "&sport=NBA")
pool <- readHTMLTable(x, stringsAsFactors = F)[["playerPoolTable"]]
pool$Points <- as.numeric(html_nodes(read_html(x), ".ptsInput") %>% html_attr("value"))

# retain relevant columns
pool <- pool[, c("Name", "Points")]
# rename columns
names(pool) <- c("Player", "Rotowire")

# save file
rotowire <- pool
save(rotowire, file = paste0("Data/", sFlag, "/Rotowire.RData"), row.names = F)