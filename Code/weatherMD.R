#Code to look at 20 years of Maryland climate data (not directly from SERC but at 3 locations nearby)
#Testing overall year-to-year variation in climate to see if summers 2002/2003 were anomalous as wet/dry years or if within typical range
#Data downloaded from NOAA in cleaned file MDWeatherData.csv

library(ggplot2)
library(lubridate)

#Reading Data, fixing some formating
data <- read.csv("MDWeatherData.csv")
weather <- data[,3:11]
weather[weather == -9999] <- NA
weather$DATE <- as.character(weather$DATE)
weather$DATE <- as.Date(weather$DATE, format = "%Y%m%d")

#Gives the station names; ATL will get dropped b/c precip data is too different.
newnames <- c("BWI", "ATL","BELT","ASS")
stations <- data$STATION_NAMES
stations <- as.character(stations)
oldnames <- unique(stations)
for(i in 1:4){ stations[stations==oldnames[i]] <- newnames[i] }
weather$STATION <- as.factor(stations)

monyr <- strftime(weather$DATE, "%Y%m") #to aggregate monthly means

#Dropping Atlantic City Data
weatherMD <- subset(weather, weather$STATION != "ATL")
weatherMD <- droplevels(weatherMD)

#Aggregate monthly weather data
MDRainMean <- aggregate(weatherMD$PRCP, by=list(weatherMD$monyr), na.rm=T,FUN=mean)
MDRainSD <- aggregate(weatherMD$PRCP, by=list(weatherMD$monyr), na.rm=T,FUN=sd)
MDLowMean <- aggregate(weatherMD$TMIN, by=list(weatherMD$monyr),na.rm=T, FUN=mean)
MDLowSD <- aggregate(weatherMD$TMIN, by=list(weatherMD$monyr),na.rm=T, FUN=sd)
MDHighMean <- aggregate(weatherMD$TMAX, by=list(weatherMD$monyr), na.rm=T,FUN=mean)
MDHighSD <- aggregate(weatherMD$TMAX, by=list(weatherMD$monyr),na.rm=T, FUN=sd)

#Building new dataframes
mdRain <- data.frame(Date=as.Date(paste(MDRainMean$Group.1, "01"), format="%Y%m%d"), Mean = MDRainMean$x, SD = MDRainSD$x)
mdLow <- data.frame(Date=as.Date(paste(MDLowMean$Group.1, "01"), format="%Y%m%d"), Mean = MDLowMean$x, SD = MDLowSD$x)
mdHigh <- data.frame(Date=as.Date(paste(MDHighMean$Group.1, "01"), format="%Y%m%d"), Mean = MDHighMean$x, SD = MDHighSD$x)

#Plotting
ggplot(mdRain, aes(x = Date, y = Mean)) + geom_ribbon(aes(ymin=Mean-SD, ymax=Mean+SD,alpha = .2)) + geom_line()

ggplot() + geom_line(data=mdLow, aes(x=Date,y=Mean), color='red') + geom_ribbon(data=mdLow, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha = .2) + geom_line(data=mdHigh, aes(x=Date,y=Mean), color='blue') + geom_ribbon(data=mdHigh, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha=.2)

#So it seems there's a fair amount of variation and 2002 and 2003 aren't anomalous
#Next part hones in summer only

s = c(5,6,7,8) #May, June, July, August
rainSummer <- subset(mdRain, is.element(month(mdRain$Date),s))
lowSummer <- subset(mdLow, is.element(month(mdLow$Date),s))
highSummer <- subset(mdHigh, is.element(month(mdHigh$Date),s))

#Plot summer; should probably add gaps between the years or use summer means, but this gets at the pretty typical year-to-year variation in rain and temp
ggplot() + geom_line(data=lowSummer, aes(x=Date,y=Mean), color='red') + geom_ribbon(data=lowSummer, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha = .2) + geom_line(data=highSummer, aes(x=Date,y=Mean), color='blue') + geom_ribbon(data=highSummer, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha=.2)
ggplot(rainSummer, aes(x = Date, y = Mean)) + geom_ribbon(aes(ymin=Mean-SD, ymax=Mean+SD,alpha = .2)) + geom_line()

