setwd('C:\\Users\\212590921\\Documents\\Personal_Projects\\Hubway_Challenge')
#graphics packages
library(ggplot2)
library(ggmap)
#library for reshaping data, calculating rolling averages
library(zoo)
#package for calculating distance between coordinates
library (geosphere)
#for mapvalues
library(plyr)
options(stringsAsFactors = F)
#get map of Boston for plotting later
get_map(geocode('Mass General Hospital, Boston'), zoom=12) -> BOSTON

#read in data
stations <- read.csv('Hubway_Stations_2011_2016.csv')
data2016_09 <- read.csv('201609-hubway-tripdata.csv')
data2016_10 <- read.csv('201610-hubway-tripdata.csv')
data2016_11 <- read.csv('201611-hubway-tripdata.csv')
data2016_12 <- read.csv('201612-hubway-tripdata.csv')
data2017_01 <- read.csv('201701-hubway-tripdata.csv')
data2017_02 <- read.csv('201702-hubway-tripdata.csv')
hub_total <- rbind(data2016_09, data2016_10, data2016_11, data2016_12, data2017_01, data2017_02)
hub_total$starttime <- strptime(hub_total$starttime, format= "%Y-%m-%d %H:%M:%S")
#arbitrarily defining 0-6 as night, 7-10 as rush hour, 11-15 as midday, 16-19 as rush hour, 20-23 as night
day_segments <- c(rep('night', 7), rep('rushhour', 4), rep('midday', 5), rep('rushhour',4), rep('night',4))
hub_total$timeofday <- mapvalues(as.numeric(format(hub_total$starttime, '%H')), from = 0:23, to=day_segments)
#because of memroy issues, have to calculate distance in a for loop
hub_total$distance <- rep(NA, nrow(hub_total))
for (i in 1:nrow(hub_total)){
  hub_total$distance[i] <- with(hub_total, distm(x=c(start.station.longitude[i], start.station.latitude[i]),
                                              y=c(end.station.longitude[i], end.station.latitude[i])))
}

#which days are the boston stations closed
unique(as.Date(subset(hub_total, end.station.id==22)$starttime))
#define winter as those days
hub_total$winter <- as.Date(hub_total$starttime)>=as.Date('2017-01-03') & as.Date(hub_total$starttime)<=as.Date('2017-02-27')

endpoints <- data.frame(table(hub_total$start.station.name, hub_total$end.station.name))

ggmap(BOSTON) + geom_text(data = subset(hub_total, start.station.latitude < 43), 
                          aes(x=start.station.longitude, y = start.station.latitude, label=start.station.id), 
                          size=1, color='orange')+ scale_size(range=c(0, 9))

hub_table <- data.frame(table(hub_total$start.station.id, hub_total$end.station.id))
#establish which stations are near the Cambridge border
cb <- c(67, 105, 179, 87, 97, 70, 143, 141, 199, 84, 143, 98, 180)
#wich station names are in cambridge
cambridge_stations <- subset(stations, Municipality == 'Cambridge')$Station

#the date in which hubway closes
t<- as.Date('2016-12-31')
cambridge1 <- subset(hub_total, start.station.id == 88 & end.station.id == 67 & usertype=='Subscriber')
cambridge1$rolling_duration <- rollmean(cambridge1$tripduration, k=9, na.pad=T)
ggplot(cambridge1) + geom_line(aes(x=starttime, y=rolling_duration))
ggplot(cambridge1) + geom_boxplot(aes(x=format(starttime, '%m'), y=tripduration))

#regression linking trip duration to age or gender
lm1 <- lm(data=cambridge1, tripduration ~ as.numeric(birth.year) + gender)
#doens't seem to show a relationship. Create a new varialb
cambridge1$older <- cambridge1$birth.year < 1967
lm2 <- lm(data=cambridge1, tripduration ~ older + gender)

cambridge2 <- subset(hub_total, start.station.id == 73 & end.station.id == 67 & usertype=='Subscriber')
cambridge2$rolling_duration <- rollmean(cambridge2$tripduration, k=9, na.pad=T)
ggplot(cambridge2) + geom_line(aes(x=starttime, y=rolling_duration))
ggplot(cambridge2) + geom_boxplot(aes(x=format(starttime, '%m'), y=tripduration))

#understand of all the trips ending in the cambridge border stations, which are the highest
which.max(subset(hub_table, Var2 %in% cb)$Freq)
subset(hub_table, Var2 %in% cb)[order(subset(hub_table, Var2 %in% cb)$Freq, decreasing = T),][1:10,]

cambridge3<- subset(hub_total, start.station.id == 67 & end.station.id == 179 & usertype=='Subscriber')
cambridge3$rolling_duration <- rollmean(cambridge3$tripduration, k=9, na.pad=T)
ggplot(cambridge3) + geom_line(aes(x=starttime, y=rolling_duration))
#crazy outlier messing the graph up
cambridge3 <- cambridge3[-394,]
cambridge3$rolling_duration <- rollmean(cambridge3$tripduration, k=9, na.pad=T)
ggplot(cambridge3) + geom_line(aes(x=starttime, y=rolling_duration))

cambridge4<- subset(hub_total, start.station.id == 68 & end.station.id == 67 & usertype=='Subscriber')
cambridge4 <- cambridge4[-92,]
cambridge4$rolling_duration <- rollmean(cambridge4$tripduration, k=9, na.pad=T)
ggplot(cambridge4) + geom_line(aes(x=starttime, y=rolling_duration))
ggplot(cambridge4) + geom_boxplot(aes(x=format(starttime, '%m'), y=tripduration))

cambridge_sub <- subset(hub_total, start.station.name %in% cambridge_stations & 
                          end.station.id %in% cb & 
                          tripduration < 5000 &
                          usertype == 'Subscriber' & gender != 0 &
                          distance != 0)
cb_lm1 <- lm(data=cambridge_sub, tripduration ~ distance + timeofday + winter)
cb_lm2 <- lm(data=cambridge_sub, tripduration ~ distance + timeofday + winter + gender)
cb_lm3 <- lm(data=cambridge_sub, log(tripduration) ~ distance + timeofday + winter + gender)

cambridge_sub[which(rownames(cambridge_sub) == 460660),]
#what goes on with the time of day
ggplot(cambridge_sub) + geom_boxplot(aes(group=as.numeric(format(starttime, '%H')), x= as.numeric(format(starttime, '%H')), y=tripduration))
