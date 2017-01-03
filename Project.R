rm(list = ls())
#R API read data
install.packages("RSocrata")
library(RSocrata)
df <- read.socrata("https://data.cityofchicago.org/resource/6zsd-86xi?$$app_token=aW2fHanT9bC9OyB5m0tqqg0Ns")
crime <- read.csv("crime.csv")
police<-read.csv("station.csv")

#clean data
library(geosphere)
crime<-subset(crime,Year>=2011)
crime <-crime[complete.cases(crime),]
write.csv(crime,"clean.csv")
distance <- vector()
for (i in 1:nrow(crime)){
  distance <- append(distance,distm(c(crime$Longitude[i],crime$Latitude[i]), c(median(crime$Longitude),median(crime$Latitude)), fun = distHaversine))
}

#map of Chicago crime
library(dplyr)
library(ggmap)
install.packages("ggplot2")
library(ggplot2)
library(devtools)
library(reshape2)
library(scales)
chicago <- get_map(location='chicago',zoom = 11)
mapdata <-crime
mapdata$Longitude  <- round(as.numeric(crime$Longitude), 2)
mapdata$Latitude <- round(as.numeric(crime$Latitude), 2)
group1 <- group_by(mapdata, Longitude, Latitude)      
sum1 <- summarise(group1,frequency=n())
chicagomap <- ggmap(chicago)
chicagomap+ geom_tile(data =sum1, aes(x = Longitude, y = Latitude, alpha = frequency),
                           fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())


#vectorize Longitude and lantitude
crimelocation <- list()
for(i in 1:nrow(crime)){
  crimelocation[[i]]=c(crime[i,"Longitude"],crime[i,"Latitude"])
}

policelocation <- list()
for(i in 1:nrow(police)){
  
  policelocation[[i]]=c(police[i,"LONGITUDE"],police[i,"LATITUDE"])
}

#Calculate the distanc between each crime location to the nearest police station
save1 <- vector()
mindis <- vector()
stationnum <- vector()

for(i in crimelocation){
  for (a in policelocation){
    save1=append(save1,distm(i, a, fun = distHaversine))
    if(length(save1)==length(policelocation)){
      mindis <-append(mindis,min(save1)) 
      stationnum <-which(save1==min(save1))
      save1 <- vector()
    }
  }
}

#Top 5 crime types
group2<- group_by(crime, Primary.Type)      
sum2 <- summarise(group2,frequency=n())
sum2 <- head(sum2[order(sum2$frequency,decreasing =T),],5)
top_5_crime <- subset(crime,Primary.Type %in% as.vector(sum2$Primary.Type))
qplot(Primary.Type,data = top_5_crime,geom = "bar",fill=Primary.Type)+scale_y_continuous(labels = comma)
colors <- c("green","orange","brown","red","blue")
barplot(sum2$frequency, names.arg = sum2$Primary.Type,xlab = "Primary.Type",ylab = "frequency",col = "blue")

#top 5 locations of crimes
group6<- group_by(crime, Location.Description)      
sum6 <- summarise(group6,frequency=n())
sum6 <- head(sum6[order(sum6$frequency,decreasing =T),],5)
top_5_location <- subset(crime, Location.Description %in% as.vector(sum6$Location.Description))
qplot(Location.Description,data = top_5_location,geom = "bar",fill=Location.Description)+scale_y_continuous(labels = comma)

#top 5 crimes by top 5 locations
top <- subset(crime, Location.Description %in% as.vector(sum6$Location.Description)&Primary.Type %in% as.vector(sum2$Primary.Type))
qplot(Primary.Type,data = top,geom = "bar",fill=Location.Description)+scale_y_continuous(labels = comma)

#top 5 violent crimes by location
violent <- c("ASSAULT","HOMICIDE","CRIM SEXUAL ASSAULT","KIDNAPPING","ROBBERY")
topv <- subset(crime, Location.Description %in% as.vector(sum6$Location.Description)&Primary.Type %in% violent)
qplot(Primary.Type,data = topv,geom = "bar",fill=Location.Description)+scale_y_continuous(labels = comma)

#top 5 violent crimes density
sub1 <- subset(crime,Year>=2011)
sub1 <- subset(crime,Primary.Type %in% violent)
sub1$Latitude <-round(sub1$Latitude,8)
sub1$Longitude <- round(sub1$Longitude,8)
group3 <- group_by(sub1,Latitude,Longitude,Primary.Type)
sum3 <- summarise(group3,frequency=n())
chicagomap +
  stat_density2d(
    aes(x = Longitude, y = Latitude, fill =  ..level..,
        alpha =  ..level..),
    size = 2, bins = 4, data = sum3,
    geom = "polygon")

#distribution of total number of crimes over last 3 years
sub7<- subset(crime,Year>=2014 & Primary.Type %in% violent)
group7 <- group_by(sub7,Latitude,Longitude,Year)
sum7 <- summarise(group7,frequency=n())
sum7 <- sum7[complete.cases(sum7),]
q1<-chicagomap +
  stat_density2d(
    aes(x = Longitude, y = Latitude, fill =  ..level..,
        alpha =  ..level..),
    size = 2, bins = 4, data = sum7,
    geom = "polygon")+scale_fill_gradient(low = "black",high= "red")+facet_grid(~Year)

#time series
time <- crime
time <- subset(time,Primary.Type=="ASSAULT"|Primary.Type=="HOMICIDE"|Primary.Type=="CRIM SEXUAL ASSAULT"|Primary.Type=="KIDNAPPING" |Primary.Type=="ROBBERY")
time$Date <-substr(as.character(time$Date),1,10)
time$Date <-as.Date(time$Date,format ="%m/%d/%Y" )
group4 <- group_by(time,Date,Primary.Type)
sum4 <-  summarise(group4,frequency=n())

library(reshape2)
df <- melt(sum4, id="Date")
ggplot(sum4,aes(x=Date,y=frequency,colour=Primary.Type,group=Primary.Type)) + geom_line()
