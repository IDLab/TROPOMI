library (ggmap)
library (RANN)
library (dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

source("scripts/1_readTropomiData.R")

filedir <- c('./bronnen')
Filenames <- list.files(path = filedir)

#define the dataframe
NO2Obs <- data.frame(Longitude = as.integer(),
                     Latitude = as.integer(),
                     Obstime = as.character(),
                     Obs = as.integer(),
                     CRFNO2Window = as.integer(),
                     stringsAsFactors = FALSE)

#Loop through all available files
for (i in 1:length(Filenames)) {
singleNO2Obs <- fReadTropomiData(Filenames[i])
NO2Obs <- rbind(NO2Obs,singleNO2Obs)
}

#clean up
rm (singleNO2Obs)

#remove NAs
NO2Obs <- NO2Obs[!is.na(NO2Obs$Obs),]

#load AIS data
ShipData <- read.csv2("data/AISData_2018-08-22_23-44-57.csv", sep = ",", stringsAsFactors = FALSE)
ShipData$Latitude <- as.numeric(ShipData$Latitude)
ShipData$Longitude <- as.numeric(ShipData$Longitude)
ShipData$TimeStamp <- as.POSIXct(ShipData$TimeStamp)
ShipData$TimeStamp <- with_tz(ShipData$TimeStamp, "UTC")
ShipData$Name <- as.factor(ShipData$Name)

#Throw out the old Dates to clean up a bit, somehow getting this trash from ImDatE
ShipData <- filter(ShipData, ShipData$TimeStamp > as.POSIXct("2018-08-10 00:00:00", tz = "UTC"))

#get a map from google
lon = mean(NO2Obs$Longitude)
lat = mean(NO2Obs$Latitude)
oceanmap <- get_map(location = c(lon, lat), zoom =4)
g <- ggmap(oceanmap)

#Filter ShipData and NO2Obs on boundix box of map
BoundingBox <- attr(oceanmap, "bb")

ShipData <- filter(ShipData, Latitude < BoundingBox$ur.lat & 
                             Latitude > BoundingBox$ll.lat & 
                             Longitude < BoundingBox$ur.lon & 
                             Longitude > BoundingBox$ll.lon)
NO2Obs <- filter(NO2Obs, Latitude < BoundingBox$ur.lat & 
                         Latitude > BoundingBox$ll.lat & 
                         Longitude < BoundingBox$ur.lon & 
                         Longitude > BoundingBox$ll.lon)


#plot  all Obs and all ShipLocs in a single map to the map
g+
  geom_point(data = NO2Obs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 0.01) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = ShipData, aes(x = Longitude, y = Latitude, shape = Name)) 

######Focus on Cape Cosmos or NSS HONESTY
CapeCosmosLoc <- filter(ShipData, Name == "CAPE COSMOS")

lon = mean(CapeCosmosLoc$Longitude)
lat = mean(CapeCosmosLoc$Latitude)
oceanmap <- get_map(location = c(lon, lat), zoom =9)
g <- ggmap(oceanmap)

BoundingBox <- attr(oceanmap, "bb")
CapeCosmosObs <- filter(NO2Obs, Latitude < BoundingBox$ur.lat & 
                           Latitude > BoundingBox$ll.lat & 
                           Longitude < BoundingBox$ur.lon & 
                           Longitude > BoundingBox$ll.lon)

#plot Obs and Locs of CAPE COSMOS
plot1<-g+
  geom_point(data = CapeCosmosObs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = CapeCosmosLoc, aes(x = Longitude, y = Latitude, shape = Name)) 

#extract nearest neighbours of known AIS locations
matches <- nn2(CapeCosmosObs[,1:2], CapeCosmosLoc[,2:3],1)
#combine Obs en Loc
CapeCosmosMatches <- cbind (CapeCosmosLoc,CapeCosmosObs[matches$nn.idx,])
CapeCosmosMatches$DeltaT <- CapeCosmosMatches[,1]-CapeCosmosMatches[,11]

CapeCosmosObs1<- unique(CapeCosmosObs[matches$nn.idx,])

plot2<- g+
  geom_point(data = CapeCosmosObs1, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, size = 5) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = CapeCosmosLoc, aes(x = Longitude, y = Latitude, shape = Name)) 

plot_grid(plot1, plot2, labels = "AUTO")

rm(temp)
