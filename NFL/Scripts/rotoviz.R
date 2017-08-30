# readHTMLTable doesn't work, so have to copy and paste data by hand into Excel spreadsheet

# read projections from rotoviz
pool <- read_excel(paste0("Data/Week ", as.character(week), "/rotoviz.xlsx"), col_names = F)
names(pool) <- c("Player", "Pos", "FP", "passYd", "passTD", "Int", "rushYd", "rushTD", "Rec", "recYd", "recTD")
pool <- pool[, -3]

# fantasy points
pool$FP <- as.numeric(as.matrix(pool[, -(1:2)]) %*% as.numeric(scoring[names(pool)[-(1:2)]]))

# rearrange columns
pool <- cbind(pool[, c("Player", "Pos", "FP")], pool[, -which(colnames(pool) %in% c("Player", "Pos", "FP"))])
# sort by fantasy points
pool <- arrange(pool, desc(FP))

# save file
rotoviz <- pool
save(rotoviz, file = paste0("./Data/Week ", week, "/rotoviz.RData"))
write.csv(rotoviz, file = paste0("./Data/Week ", week, "/rotoviz.csv"), row.names = F)