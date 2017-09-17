library(dplyr)
library(ggplot2)

options(stringsAsFactors = F)
setwd("C:/DFS/DFS_datalytics/Projections/NFL")

load("week2_proj.RData")

# build a better label to put on charts (include projection points)
ds2$plot.nm <- paste0(ds2$dk.name, " (", ds2$DK.PTS, ")")

# Use this to load the proper postion data into the ds.plot variable that will be used for all the plots
#ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos != "DEF")
ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos == "QB")
ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos == "RB")
ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos == "WR")
ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos == "TE")
ds.plot <- filter(ds2, DK.pts.per.1000 >= 2 & pos == "DEF")

# min and max for x and y to draw the lines
mn.x <- min(ds.plot$Salary)
mx.x <- max(ds.plot$Salary)
mn.y <- min(ds.plot$DK.pts.per.1000)
mx.y <- max(ds.plot$DK.pts.per.1000)
mid.x <- (mn.x + mx.x) / 2
mid.y <- (mn.y + mx.y) / 2
ds.plot$mid.x <- mid.x
ds.plot$mid.y <- mid.y
ds.plot$type <- NA
# Give the 4 quadrants labels
ds.plot[ds.plot$Salary >= ds.plot$mid.x & ds.plot$DK.pts.per.1000 >= ds.plot$mid.y, "type"] <- "Power"
ds.plot[ds.plot$Salary < ds.plot$mid.x & ds.plot$DK.pts.per.1000 >= ds.plot$mid.y, "type"] <- "Value"
ds.plot[ds.plot$Salary >= ds.plot$mid.x & ds.plot$DK.pts.per.1000 < ds.plot$mid.y, "type"] <- "Avoid"
ds.plot[ds.plot$Salary < ds.plot$mid.x & ds.plot$DK.pts.per.1000 < ds.plot$mid.y, "type"] <- "Risky"

# create the plot
ggplot(ds.plot, aes(x = Salary, y= DK.pts.per.1000, label = plot.nm, color = type)) + 
  geom_text(size = 5) +
  geom_vline(xintercept = mid.x) + geom_hline(yintercept = mid.y) + theme_bw() +
  ggtitle("DK points per 1000 (value) vs Salary (multiplyer)") +
  scale_colour_manual(values = c("red", "dodgerblue4", "orange", "forestgreen")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", label = "CAUTION", x = mx.x, y = mn.y * 0.95, size = 8, colour = "black") +
  annotate("text", label = "RISKY", x = mn.x, y = mn.y * 0.95, size = 8, colour = "black") +
  annotate("text", label = "POWER", x = mx.x, y = mx.y * 1.05, size = 8, colour = "black") +
  annotate("text", label = "VALUE", x = mn.x, y = mx.y * 1.05, size = 8, colour = "black") +
  theme(legend.position="none")








