library(RSQLite)
library(reshape)
library(plyr)
library(ggplot2)


setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

tree <- dbGetQuery(db, "SELECT * FROM TreeData")

dbDisconnect(db)

tree$BasalArea <- (tree$DBH/2)^2*0.0001*pi
tree$Species <- substr(tree$TreeID, 1,2)

source("multiplot.R")

p1 <- ggplot(tree, aes(x=Elevation, fill=Species)) + geom_density(alpha=0.5) + ggtitle("Species distribution by elevation")

p2 <- ggplot(tree, aes(x=Elevation, y=BasalArea, color=Species)) + geom_point() + stat_smooth(method="loess", formula=y~x, size=1, alpha = .2) + coord_cartesian(ylim=c(0, .9)) + xlab("Elevation (m)") + ylab(bquote("Basal Area ("*m^2*")")) + ggtitle("Basal Area vs Elevation (with Loess smoother)")

multiplot(p1, p2)
