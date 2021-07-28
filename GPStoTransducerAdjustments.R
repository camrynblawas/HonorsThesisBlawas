library(dplyr)
library(tidyverse)
library(rgdal)
library(ggplot2)

#options(max.print=1000000)
#References
##https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm

#Import heading
heading.raw <- read.csv("~/HonorsThesisData/AR285Heading.heading.csv")

#Import cruisetrack csv using GPS locations in lat and long
d <- read.csv("~/HonorsThesisData/HonorsThesisGIS/CruiseTracks/AR285csv.gps.csv")
df <- as.data.frame(d)

#Transform Lat and Long to UTM (check x and y) (long = easting and lat = northing idk why)
coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(d, CRS("+proj=utm +zone=18 ellps=WGS84"))
utm <- as(res, "SpatialPoints")
utm.df <- as.data.frame(utm)
names(utm.df)[names(utm.df)=="Longitude"] <- "Easting"
names(utm.df)[names(utm.df)=="Latitude"] <- "Northing"
head(utm.df)

#Get only one heading per second
heading <- heading.raw[!duplicated(heading.raw$Heading_time),]

#Rename time
heading$time <- heading$Heading_time

#Merge utm and latlong 
GPS <- cbind(df, utm.df)

#Rename time in GPS
GPS$time <- GPS$GPS_time

#Add offsets for GPS/Transducer offset in meters
x <- -5.593
y <- -1.564

#Find vector of those values
diagonal = sqrt((x^2) + (y^2))
#Find angle between transducer and diagonal vector
beta = (atan(y/x))*180/pi
#x adjustment value using heading
xoffset <- (sin(heading$Bearing - beta)*(pi/180))*diagonal
as.data.frame(xoffset)
#y adjustment value using heading
yoffset <- (cos(heading$Bearing - beta)*(pi/180))*diagonal
as.data.frame(yoffset)


#Adjust UTM Coordinates to take in adjustments for GPS/transducer offset
GPS2 <- mutate(GPS, "Easting" = Easting + x, "Northing" = Northing + y)

#Match heading and GPS data and remove errors (didn't remove NAs??)
GPS2 <- merge(x = GPS2, y = heading, by = "time", all = TRUE)
GPS2 <- drop_na(GPS2)


#plot to check
utm.adjusted <- select(GPS2, "Easting","Northing")
ggplot()+
  geom_point(data = utm.adjusted, mapping = aes(Easting, Northing), size = 0.05, color = 'blue')+
  geom_point(data = utm.df, mapping =aes(Easting, Northing), size = 0.05, color = 'red')

#Select important columns
GPS.final <- select(GPS2, -"Latitude", -"Longitude", -"time")

#UTM back to Lat Long
sputm <- SpatialPoints(GPS.final, proj4string=CRS("+proj=utm +zone=18S +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

write.csv(GPS.final, "AR285Adjusted.csv")
