setwd('~/Documents/CAL/Real_Life/Logical/Hubway_Challenge')
library(ggplot2)
library(ggmap)
library(zoo)
options(stringsAsFactors = F)
get_map(geocode('Mass General Hospital, Boston'), zoom=12) -> BOSTON

ggmap(BOSTON) + geom_point(data=subset(crime2015, OFFENSE.CODE.GROUP  %in% c("Bomb Hoax", "Confidence Games", "Larceny", "Robbery")), aes(x=LONG, y=LAT, color= OFFENSE.CODE.GROUP), size= 0.1) + scale_size(range=c(0, 0.5))

data2016_11 <- read.csv('201611-hubway-tripdata.csv')
data2016_12 <- read.csv('201612-hubway-tripdata.csv')
data2017_01 <- read.csv('201701-hubway-tripdata.csv')
data2017_02 <- read.csv('201702-hubway-tripdata.csv')
hub_total <- rbind(data2016_11, data2016_12, data2017_01, data2017_02)
hub_total$starttime <- strptime(hub_total$starttime, format= "%Y-%m-%d %H:%M:%S")

endpoints <- data.frame(table(hub_total$start.station.name, hub_total$end.station.name))

ggmap(BOSTON) + geom_text(data = subset(hub_total, start.station.latitude < 43), aes(x=start.station.longitude, y = start.station.latitude, label=start.station.id), size=1, color='orange')+ scale_size(range=c(0, 3))

hub_table <- data.frame(table(hub_total$start.station.id, hub_total$end.station.id))
#establish which stations are near the Cambridge border
cb <- c(67, 105, 179, 87, 97, 70, 143, 141, 199, 84, 143, 98, 180)
#the date in which hubway closes
t<- as.Date('2016-12-31')
cambridge1 <- subset(hub_total, start.station.id == 88 & end.station.id == 67 & usertype=='Subscriber')
cambridge1$rolling_duration <- rollmean(cambridge1$tripduration, k=9, na.pad=T)
ggplot(cambridge1) + geom_line(aes(x=starttime, y=rolling_duration))
ggplot(cambridge1) + geom_boxplot(aes(x=format(starttime, '%m'), y=tripduration))

cambridge2 <- subset(hub_total, start.station.id == 73 & end.station.id == 67 & usertype=='Subscriber')
cambridge2$rolling_duration <- rollmean(cambridge2$tripduration, k=9, na.pad=T)
ggplot(cambridge2) + geom_line(aes(x=starttime, y=rolling_duration))
ggplot(cambridge2) + geom_boxplot(aes(x=format(starttime, '%m'), y=tripduration))