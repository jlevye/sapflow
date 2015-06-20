library(RSQLite)
library(reshape)
library(plyr)
library(ggplot2)

setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

#Creating isotope table - did once, don't run again else error!
iso <- read.csv("CSVDBTables/Isotopes.csv")
iso$CNRatio <- as.numeric(as.character(iso$CNRatio))
iso$JulDay <- as.numeric(as.character(iso$JulDay))
dbWriteTable(db, "isotope",iso)

#Data for analysis/plotting
source(multiplot.R)

iso$species <- substr(iso$TreeID, 1,2)
iso$Year <- as.factor(iso$Year)
p1 <- ggplot(iso, aes(x=JulDay, y=d13C, color=species)) + geom_point() + facet_grid(Year~.)
m <- lm(d13C ~ species+Year+JulDay + Year:JulDay, data=iso)
summary(m)
m.a <- update(m, ~.-Year:JulDay)
plot(allEffects(m.a)) #Shoes the relationships more clearly

#Add Elevation
iso.el <- dbGetQuery(db, "SELECT isotope.Year, isotope.JulDay, isotope.TreeID, isotope.d13C, TreeData.TreeID, TreeData.Elevation FROM isotope INNER JOIN TreeData ON isotope.TreeID = TreeData.TreeID")
iso.el$Year <- as.factor(iso.el$Year)
iso.el$Species <- as.factor(substr(iso.el$TreeID,1,2))
iso.el <- iso.el[,-5]
p2 <- ggplot(iso.el, aes(x=Elevation, y=d13C, color=Species)) + facet_grid(Year~.) + geom_point()

for (i in 1:47){
  name <- treeids[i]
    for (j in 1:nrow(subset(iso, iso$TreeID==name))){
      day <- iso$JulDay[j]
      if (is.element(day, soil.a$Day[which(soil.a$TreeID==name)])) {
        iso$moist[which(iso$JulDay==day & iso$TreeID==name)] <- soil.a$WaterContent[which(soil.a$JulDay==day & soil.a$TreeID==name)]
        print("iso$moist set")
      }
      else {
        for (k in 1:10) {
          if (is.element(day-k, soil.a$Day[which(soil.a$TreeID==name)])) {
            iso$moist[which(iso$JulDay==day & iso$TreeID==name)] <- soil.a$WaterContent[which(soil.a$JulDay==day-k & soil.a$TreeID==name)]
            print("iso$moist set")
            break
        }
      }
    }
  }
}


