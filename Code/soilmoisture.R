#Soil Moisture Processing code
#Pulling from SapFlow DB, data processing
#

library(RSQLite)
library(plyr)
library(effects)
library(ggplot2)

#Get the data needed
setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

dbSendQuery(db, "ALTER TABLE soilmoisture ADD COLUMN WaterContent")
dbSendQuery(db, 'UPDATE soilmoisture SET WaterContent = 0.31217*log(TDR-7.64) - 0.3912')

soilFull <- dbGetQuery(db, "SELECT soilmoisture.Year, soilmoisture.JulDay, soilmoisture.TreeID, soilmoisture.WaterContent, TreeData.TreeID, TreeData.Elevation FROM soilmoisture INNER JOIN TreeData on soilmoisture.TreeID = TreeData.TreeID")

#Processing data pulled from DB
soilFull <- soilFull[,-5] #Deletes duplicate column
soil.a <-  aggregate(cbind(soilFull$WaterContent, soilFull$Elevation), list(Day=soilFull$JulDay, TreeID = soilFull$TreeID, Year = soilFull$Year), FUN=mean)
names(soil.a)[4:5] <- c("WaterContent", "Elevation")
#Water table; high = upland
soil.a$WT <- as.factor(ifelse(soil.a$Elevation < 10, "Low", ifelse(soil.a$Elevation > 15, "High", "Mid")))
soil.a$Year <- as.factor(soil.a$Year)
soil.b <- subset(soil.a, soil.a$WT != "Mid")

#Models and Plotting
#Elevation as continuous variable
m1 <- lm(WaterContent ~ Year*Elevation*Day, data = soil.a)
summary(m1)
plot(allEffects(m1))
ggplot(soil.a) + geom_point(aes(Elevation,WaterContent, color = Year))
ggplot(soil.a) + geom_point(aes(Day,WaterContent, color = Year))

#Elevation as factor (WT)
m2 <- lm(WaterContent~ WT*Year*Day, data = soil.b)
summary(m2) #Showed day not significant (either on its own or interacting), so simplify:
m3 <- lm(WaterContent ~ WT*Year, data = soil.b )
summary(m3)
plot(allEffects(m3))

#Bar Graphs
summ <- ddply(soil.b, .(Year, WT), summarize, mean = mean(WaterContent), sd = sd(WaterContent))
ggplot(summ, aes(Year, mean, fill=WT)) + geom_bar(position = position_dodge(), stat="identity") + geom_errorbar(aes(ymin=mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(0.9))

#Future analysis goes here

#Close DB connection
dbDisconnect(db)
