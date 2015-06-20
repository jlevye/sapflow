#DB processing code template
library(RSQLite)
#any additional libraries needed for analysis
library(lubridate)
library(ggplot2)
source("~/Documents/School/GradSchool/Sap Flow/multiplot.R")

#Connect the DB
setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

#Pull data with - fill in missing values
dbSendQuery(db, "ALTER TABLE weather ADD COLUMN VPSat")
dbSendQuery(db, "ALTER TABLE weather ADD COLUMN VPD")
dbSendQuery(db, "ALTER TABLE weather ADD COLUMN adjSolar")
dbSendQuery(db, 'UPDATE weather SET
    VPSat = 6.11 * exp(17.27*Temp/(237.3*Temp)),
    adjSolar = 
      CASE WHEN Solar > 0 THEN Solar/(.109409*6.452*power(10, -6))
      ELSE 0
    ')
dbSendQuery(db, "UPDATE weather SET VPD = (1-RH/100)*VPSat")

#When done:
dbDisconnect(db)

#Playing With Solar Data
sun <- dbGetQuery(db, "SELECT Year, JulDayEST as Day, TimeEST as Time, Solar FROM weather")
sun<- subset(sun, sun$Solar < 2000)
sun$Solar <- ifelse(sun$Solar < 0, 0, sun$Solar)
sun$Year <- as.factor(sun$Year)
sun$Time <- as.character(sun$Time)
sun$Hour <- ifelse(nchar(sun$Time) == 2, 0, ifelse(nchar(sun$Time)==3, substr(sun$Time,1,1), substr(sun$Time, 1,2)))
sun$Min <- ifelse(nchar(sun$Time)==2, sun$Time, ifelse(nchar(sun$Time)==3, substr(sun$Time,2,3), substr(sun$Time,3,4)))

hourly <- aggregate(sun$Solar, by = list(sun$Year, sun$Day, sun$Hour), FUN=mean)
names(hourly) <- c("Year", "Day", "Hour", "Solar")
hourly$stringdatetime <- paste(as.character(hourly$Day),as.character(hourly$Hour))
hourly$datetime <- as.POSIXct(hourly$stringdatetime, format="%j %H")
hourly$Week <- floor((hourly$Day - min(hourly$Day))/7)
hourly$Month <- month(hourly$datetime)

ggplot(hourly, aes(x = datetime, y = Solar)) + geom_point(aes(color = Year)) + facet_wrap(~ Week, ncol=4, scales="free_x")
ggplot(hourly, aes(x = datetime, y = Solar)) + geom_point(aes(color = Year)) + facet_wrap (~ Month, ncol=2, scale="free_x")
max <- aggregate(hourly$Solar, by = list(hourly$Year, hourly$Day), FUN=max)
names(max) <- c("Year", "Day","MaxSolar")
ggplot(max, aes(x =Day, y=MaxSolar)) + geom_point(aes(color=Year)) + xlab("Day of Year") + ylab(bquote("Max Solar Radiation (W "*m^-2*")"))

sunmax2002 <- subset(max, max$Year == 2002)
sunmax2003 <- subset(max, max$Year == 2003)
sunmax2002 <- sunmax2002[,2:3]
sunmax2003 <- sunmax2003[,2:3]
names(sunmax2002)[2] <- "Max2002"
names(sunmax2003)[2] <- "Max2003"
sunmax <- merge(sunmax2002,sunmax2003, by="Day")
t.test(sunmax$Max2002, sunmax$Max2003, paired=TRUE)



#Comparing Precip Data
rain <- dbGetQuery(db,"SELECT Year, JulDayEST as Day, TimeEST as Time, Rain FROM weather")
daily <- aggregate(rain$Rain, by=list(rain$Year, rain$Day), FUN=sum)
names(daily) <- c("Year","Day","Precip")
daily$Year <- as.factor(daily$Year)
ggplot(daily, aes(x = Day, y = Precip)) + geom_point(aes(color=Year)) + xlab("Day of Year") + ylab("Precip (mm)")

totalrain <- setNames(aggregate(daily$Precip, by = list(daily$Year), FUN=sum), c("Year","Precip"))
ggplot(totalrain, aes(x = Year, y = Precip)) + geom_bar()

daily$RainBin <- ifelse(daily$Precip == 0, 0, 1)
daily$RainBin <- as.factor(daily$RainBin)
ggplot(daily, aes(x=as.factor(Year), fill=RainBin)) + geom_bar(position="fill") + labs(x="Year",y="", title="Proportion of Days Without Rain") + scale_fill_discrete(name="Rain?",labels=c("No","Yes"))

avgs <- ddply(daily, .(Year), summarize, mean=mean(Precip, na.rm=T), sd = sd(Precip, na.rm=T))
ggplot(avgs, aes(x=as.factor(Year),y=mean, fill=Year)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=0, ymax=mean+sd), width=0.25) + labs(title="Mean Daily Precip", x="Year",y="Precip (in)")



#Comparing Temp Data
temp <- dbGetQuery(db, "SELECT Year, JulDayEST as Day, TimeEST as Time, Temp FROM weather")
maxTemp <- aggregate(temp$Temp, by=list(temp$Year, temp$Day), FUN=max)
minTemp <- aggregate(temp$Temp, by=list(temp$Year, temp$Day), FUN=min)
names(maxTemp) <- c("Year", "Day", "MaxTemp")
names(minTemp) <- c("Year", "Day", "MinTemp")
minTemp$Year <- as.factor(minTemp$Year)
maxTemp$Year <- as.factor(maxTemp$Year)
g1 <- ggplot(minTemp, aes(x = Day, y = MinTemp)) + geom_point(aes(color=Year)) + xlab("Day of Year") + ylab("Temp ("~degree~"C)") +ggtitle("Min. Daily Temperature")
g2 <- ggplot(maxTemp, aes(x = Day, y = MaxTemp)) + geom_point(aes(color=Year)) + xlab("Day of Year") + ylab("Temp ("~degree~"C)") +ggtitle("Max. Daily Temperature")
multiplot(g1,g2, cols=2) #See source file; from R-Cookbook website

maxTemp2002 <- subset(maxTemp, maxTemp$Year == 2002)[,2:3]
maxTemp2003 <- subset(maxTemp, maxTemp$Year == 2003)[,2:3]
names(maxTemp2002)[2] <- "Max2002"
names(maxTemp2003)[2] <- "Max2003"

minTemp2002 <- subset(minTemp, minTemp$Year == 2002)[,2:3]
minTemp2003 <- subset(minTemp, minTemp$Year == 2003)[,2:3]
names(minTemp2002)[2] <- "Min2002"
names(minTemp2003)[2] <- "Min2003"

pMaxTemp <- merge(maxTemp2002, maxTemp2003, by="Day")
pMinTemp <- merge(minTemp2002, minTemp2003, by="Day")

t.test(pMaxTemp$Max2002, pMaxTemp$Max2003, paired=TRUE)
t.test(pMinTemp$Min2002, pMinTemp$Min2003, paired=TRUE)


#Comparing VPD
vpd <- dbGetQuery(db,"SELECT Year, JulDayEST as Day, TimeEST as Time, VPD FROM weather")
vpd$Year <- as.factor(vpd$Year)
library(stringr)
vpd$Time <- as.character(vpd$Time)
vpd$Time <- str_pad(vpd$Time, 4, "left", "0")
vpd$DateTime <- as.POSIXct(paste(as.character(vpd$Day), vpd$Time), format="%j %H%M")
hourlyvpd <- setNames(aggregate(vpd$VPD, by = list(vpd$Year, vpd$Day, hour(vpd$DateTime)), FUN=mean), c("Year","Day","Hour","VPD"))
hourlyvpd$DateTime <- ISOdatetime(year=0, month=month(as.POSIXct(as.character(hourlyvpd$Day), format="%j")), day = day(as.POSIXct(as.character(hourlyvpd$Day), format="%j")), hour=hourlyvpd$Hour, min=0, sec=0)
ggplot(hourlyvpd, aes(x = DateTime, y = VPD, color=Year)) + geom_point() + xlab("Date and Time (hourly)")

