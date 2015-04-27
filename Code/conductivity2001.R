library(RSQLite)
library(reshape)
library(plyr)
library(ggplot2)
#any additional libraries needed for analysis

#Connect the DB
setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

#Cleaning up the conductivity table from original input - RAN ONCE, DON"T RUN AGAIN
cond <- dbGetQuery(db, "SELECT * FROM conductivity")
cond <- cond[2:nrow(cond),] #Drops messed up first row
cond$Date <- as.Date(cond$Date, "%m/%d/%Y")
l <- c(11.6, 12.1, 9.9, 11.6, 9.6, 9.6, 7.9, 16.9, 7.7, 6.1, 11.1, 11.6, 6, 7.1, 9.3, 7.1, 11.9, 8.4, 9.1, 7.2, 10.7, 9.8, 7.5, 12.2,7.8, NA, NA)
cond$Length <- h
cond$HuberValue <- cond$SA/cond$LeafArea
cond$dH <- (cond$CarboyHT - cond$BalanceHT)/100
cond$PlantCond <- cond$Flow*(cond$LeafLength/100)/cond$dH*10
cond$Ks <- cond$PlantCond/cond$SA
cond$LSC <- cond$PlantCond/cond$LeafArea

dbSendQuery(db, "DROP TABLE conductivity")
dbWriteTable(db, "conductivity", cond)

#Done with db
dbDisconnect(db)

#Data Reshaping and plotting
cond$Species <- substr(cond$TreeID, 1,2)
shaped <- melt(cond[,2:ncol(cond)], id.vars=c("TreeID","Species"))
pafa <- subset(shaped, is.element(shaped$Species, c("PA", "FA")))
pafa<- ddply(pafa, .(Species, variable), summarise, mean = mean(value, na.rm=T), sd = sd(value,na.rm=T) )
pafa <- subset(pafa, is.element(pafa$variable, c("PlantCond","HuberValue","Ks","LSC")))
pafa$Title <- c("Huber Value","Ks", "Leaf Specific Conductivity","Plant Conductivity")

ggplot(pafa, aes(x=Species, fill=Species, y=mean)) + facet_wrap(~Title, scale="free") + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.25)
