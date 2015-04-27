#Code to look at 20 years of Maryland climate data (not directly from SERC but at 3 locations nearby)
#Testing overall year-to-year variation in climate to see if summers 2002/2003 were anomalous as wet/dry years or if within typical range
#Data downloaded from NOAA in cleaned file MDWeatherData.csv

library(ggplot2)
library(lubridate)
library(plyr)
library(reshape)

#Reading Data, fixing some formating
data <- read.csv("CSVDBTables/MDWeatherData.csv")
weather <- data[,3:11]
weather[weather == -9999] <- NA
weather$DATE <- as.character(weather$DATE)
weather$DATE <- as.Date(weather$DATE, format = "%Y%m%d")

#Gives the station names; ATL will get dropped b/c precip data is too different.
newnames <- c("BWI", "ATL","BELT","ASS")
stations <- data$STATION_NAME
stations <- as.character(stations)
oldnames <- unique(stations)
for(i in 1:4){ stations[stations==oldnames[i]] <- newnames[i] }
weather$STATION <- as.factor(stations)

monyr <- strftime(weather$DATE, "%Y%m") #to aggregate monthly means

#Dropping Atlantic City Data
weatherMD <- subset(weather, weather$STATION != "ATL")
weatherMD <- droplevels(weatherMD)
weatherMD$Month <- month(weatherMD$DATE)
weatherMD$Year <- year(weatherMD$DATE)
weatherMD <- weatherMD[,-c(1:3,10)]

#Setting up monthly data
monthly <- weatherMD[,-c(1:3)]
monthly <- melt(monthly, id.vars <- c("Month", "Year"))

rain <- subset(monthly, monthly$variable=="PRCP")
temp <- subset(monthly, monthly$variable != "PRCP")
rain <- ddply(rain, .(Year, Month), summarize, Total = sum(value, na.rm=T))
temp$value <- temp$value/10
temp1 <- ddply(temp, .(Month, Year, variable), summarize, mean= mean(value, na.rm=T), sd = sd(value, na.rm=T))
temp$date <- ISOdate(year=temp$Year, month=temp$Month, day=1)
temp$flag <- ifelse(temp$variable=="TMIN" & (temp$Year==2002 | temp$Year==2003), 3, ifelse(temp$variable=="TMIN" & temp$Year != 2002 & temp$Year != 2003, 2, ifelse(temp$variable=="TMAX"& (temp$Year==2002|temp$Year==2003),1,0)))
temp$flag <- as.factor(temp$flag)
ggplot(temp, aes(x=date, y=mean, color=flag, group=variable)) + geom_line() + theme(legend.position="none") + labs(x="Date",y="Avg. Temp (deg.C)", title = "Average Min and Max Temperature, 1990-2010")




#Rain Data
rain$date <- ISOdate(year=rain$Year, month=rain$Month, day=1)
rain$flag <- ifelse(rain$Year==2002 | rain$Year==2003, 1, 0)
rain$flag <- as.factor(rain$flag)
rain$group<- ifelse(rain$Year<2002,0,ifelse(rain$Year>2003,1,2))
rain$Total <- rain$Total/1000 #Could have done this step earlier

ggplot(rain, aes(x=date, y=Total, color=flag, group=group)) + geom_line() + labs(x="Date",y="Precip (mm)", title="Monthly Precipitation 1990-2010") + theme(legend.position="none")

#Temp


#So it seems there's a fair amount of variation and 2002 and 2003 aren't anomalous
#Next part hones in summer only

s = c(5,6,7,8) #May, June, July, August
rainSummer <- subset(mdRain, is.element(month(mdRain$Date),s))
lowSummer <- subset(mdLow, is.element(month(mdLow$Date),s))
highSummer <- subset(mdHigh, is.element(month(mdHigh$Date),s))

#Plot summer; should probably add gaps between the years or use summer means, but this gets at the pretty typical year-to-year variation in rain and temp
ggplot() + geom_line(data=lowSummer, aes(x=Date,y=Mean), color='red') + geom_ribbon(data=lowSummer, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha = .2) + geom_line(data=highSummer, aes(x=Date,y=Mean), color='blue') + geom_ribbon(data=highSummer, aes(x=Date,ymin=Mean-SD, ymax=Mean+SD), alpha=.2)
ggplot(rainSummer, aes(x = Date, y = Mean)) + geom_ribbon(aes(ymin=Mean-SD, ymax=Mean+SD,alpha = .2)) + geom_line()

