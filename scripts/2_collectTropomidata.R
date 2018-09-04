library (ggmap)
library (RANN)
library (dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library (geosphere)

source("scripts/1_readTropomiData.R")
source("1_collectSingleShipObs")

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


##########
#load AIS data
ShipData <- read.csv2("./data/AISData_2018_08_31_12_52_17.csv", sep = ",", stringsAsFactors = FALSE)
ShipData$Latitude <- as.numeric(ShipData$Latitude)
ShipData$Longitude <- as.numeric(ShipData$Longitude)
ShipData$Heading <- as.numeric(ShipData$Heading)
ShipData$TimeStamp <- as.POSIXct(ShipData$TimeStamp, tz = "UTC")
ShipData$Name <- as.factor(ShipData$Name)
#sometimes there is double entries ?!
ShipData <- unique(ShipData)

#plot all Ships in AISdata
if (FALSE) {
  lon = mean(ShipData$Longitude)
  lat = mean(ShipData$Latitude)
  oceanmap <- get_map(location = c(lon, lat), zoom =2)
  g <- ggmap(oceanmap)
  g+
    geom_point(data = ShipData, aes(x = Longitude, y = Latitude)) 
}

#get a map from google
lon = mean(NO2Obs$Longitude)
lat = mean(NO2Obs$Latitude)
oceanmap <- get_map(location = c(lon, lat), zoom =4)
g <- ggmap(oceanmap)

#Filter ShipData and NO2Obs on boundix box of map
BoundingBox <- attr(oceanmap, "bb")

filterShipData <- filter(ShipData, Latitude < BoundingBox$ur.lat &
                             Latitude > BoundingBox$ll.lat &
                             Longitude < BoundingBox$ur.lon &
                             Longitude > BoundingBox$ll.lon)
#For performance one could filter also the TROPOMI data for chosen window
# NO2Obs <- filter(NO2Obs, Latitude < BoundingBox$ur.lat & 
#                          Latitude > BoundingBox$ll.lat & 
#                          Longitude < BoundingBox$ur.lon & 
#                          Longitude > BoundingBox$ll.lon)
#write.csv(NO2Obs, file = "FilteredNO2Obs.csv")

################
#This part of code will map ships and TROPOMI observations. Allowing to zoom in on one particular ship
###########

#plot  all Obs and all ShipLocs in a single map to the map
if (FALSE) {
  g+
    geom_point(data = NO2Obs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 2) +
    scale_color_gradientn(colours = terrain.colors(7)) +
    geom_point(data = ShipData, aes(x = Longitude, y = Latitude))
}

################
#Call function to get individual ship data
fCollectSingleShipObsList <- fCollectSingleShipObs(NO2Obs, ShipData, ShipName)
SingleShipSurround1 <- fCollectSingleShipObsList$SingleShipSurround1
bestSingleShipLoc1 <- fCollectSingleShipObsList$bestSingleShipLoc
##############

######Focus LEOPOLD OLDENDORFF
SingleShipLoc <- filter(ShipData, Name == "LEOPOLD OLDENDORFF")

lon = mean(SingleShipLoc$Longitude)
lat = mean(SingleShipLoc$Latitude)
oceanmap <- get_map(location = c(lon, lat), zoom =9)
g <- ggmap(oceanmap)

BoundingBox <- attr(oceanmap, "bb")
SingleShipObs <- filter(NO2Obs, Latitude < BoundingBox$ur.lat & 
                           Latitude > BoundingBox$ll.lat & 
                           Longitude < BoundingBox$ur.lon & 
                           Longitude > BoundingBox$ll.lon)

#extract nearest neighbours observations of known AIS locations
matches <- nn2(SingleShipObs[,1:2], SingleShipLoc[,2:3],1)
#combine Obs en Loc
SingleShipMatches <- cbind (SingleShipLoc,SingleShipObs[matches$nn.idx,])
SingleShipMatches$DeltaT <- SingleShipMatches[,1]-SingleShipMatches[,11]

# Select the rows with the lowest timedifference
best <- which.min(abs(SingleShipMatches$DeltaT))
nextbest <- nn2(SingleShipMatches$DeltaT, SingleShipMatches$DeltaT[best],2)$nn.idx[2]

bestSingleShipLoc <- filter (SingleShipLoc, TimeStamp == SingleShipMatches$TimeStamp[best])

#########
#plot Obs and Locs of SingleShip
plot1<-g+
  geom_point(data = SingleShipObs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = SingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name)) +
  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 5) 

#####
#Interpolate to increase matching the right spot

#build a data.frame for clarity
nrows <- 150
SingleShipInterpol <- as.data.frame(matrix(NA,nrows,3))
colnames(SingleShipInterpol) <- c("time", "Latitude", "Longitude")

#make series to match from
SingleShipInterpol$time <- seq (SingleShipMatches$TimeStamp[best], SingleShipMatches$TimeStamp[nextbest], length.out = nrows)
SingleShipInterpol$Latitude <- seq (SingleShipMatches$Latitude[best], SingleShipMatches$Latitude[nextbest], length.out = nrows)
SingleShipInterpol$Longitude <- seq (SingleShipMatches$Longitude[best], SingleShipMatches$Longitude[nextbest], length.out = nrows)

#find the best matching interpolated time
idx <- nn2(SingleShipInterpol$time, SingleShipMatches$time[best],1)$nn.idx

#Start with original best match, update with new tim, lat and long
bestSingleShipLoc$TimeStamp <- SingleShipInterpol$time[idx]
bestSingleShipLoc$Latitude <-SingleShipInterpol$Latitude[idx]
bestSingleShipLoc$Longitude <- SingleShipInterpol$Longitude[idx]
# add Origin as opposite of Heading
bestSingleShipLoc$Origin <- ifelse(bestSingleShipLoc$Heading[1]>180,bestSingleShipLoc$Heading[1] - 180,bestSingleShipLoc$Heading[1] + 180)

#clean up mess of intermediate vars
rm(nrows,best, nextbest, idx, SingleShipInterpol)

############
#Match surrounding cells with bestSingleShipLoc, calculate diff in concentrations and plot

#first determine which obs are surrounding the ship
matches <- nn2(SingleShipObs[,1:2], bestSingleShipLoc[,2:3],100)
SingleShipSurround <- SingleShipObs[matches$nn.idx,]
SingleShipSurround$DistToOrg <- as.vector (matches$nn.dists)
#determine for each point the difference in Obs conc with the cell where the ship is at >0 means higher than where ship is. 
refConc <-  SingleShipSurround$Obs[which(SingleShipSurround$DistToOrg == min(SingleShipSurround$DistToOrg))]
SingleShipSurround$DeltaConc <- SingleShipSurround$Obs-refConc
# calculate the bearing of each cell surrounding the ship, to later assign sector in polar plot
origin <-c(bestSingleShipLoc$Longitude,bestSingleShipLoc$Latitude)
SingleShipSurround$Bearing <- sapply(1:nrow(SingleShipSurround), function (x) {bearingRhumb(origin,c(SingleShipSurround$Longitude[x],SingleShipSurround$Latitude[x]))})

##assign Polar sector and Distance sector to the Obs surrounding the ship
#YOU MUST USE AN EVEN NUMBER OF SECTORS, CURRENTLY ALLIGNMENT OF SECTORS IS BASED ON HEADING, NOT ORIGIN
step <- 360/20
BearingSectors <- seq(step/2,360-step/2, by = step)
#modify sectors so that centre of a sector will match the ships heading 
#THIS IS NOT WORKING FOR THE FIRST AND LAST NORTHERN SECTOR YET
nearest <- nn2(BearingSectors, bestSingleShipLoc$Origin,2)
dif <- (BearingSectors[nearest$nn.idx[1]]-BearingSectors[nearest$nn.idx[2]])/2
BearingShift <- ifelse (dif >= 0, dif - nearest$nn.dists[1], dif + nearest$nn.dists[1])
BearingSectors <- BearingSectors + BearingShift
#define the sectors with respect to distance
DistanceSectors <- seq(min(SingleShipSurround$DistToOrg),max(SingleShipSurround$DistToOrg), length.out = 10)
#assign default value for Distance and Polar sector
SingleShipSurround$PolarSector <- rep (last(BearingSectors),nrow(SingleShipSurround))
SingleShipSurround$DistanceSector <- rep (1,nrow(SingleShipSurround))

#assign for all surrounding points
for (i in 1:(length(BearingSectors)-1)) {
  SingleShipSurround$PolarSector[between(SingleShipSurround$Bearing, BearingSectors[i], BearingSectors[i+1])] <- BearingSectors[i]
  SingleShipSurround$DistanceSector[between(SingleShipSurround$DistToOrg, DistanceSectors[i], DistanceSectors[i+1])] <- i
}

#Build datastructure to plot in polar, each sector has average of DeltaConc in area
RadialPlotData<-SingleShipSurround %>%
  group_by(.dots= c("PolarSector","DistanceSector")) %>%
  summarize(meanDeltaConc = mean(DeltaConc, na.rm = TRUE))

CoursePlot <- as.data.frame(matrix(NA,1,1))
colnames(CoursePlot) <- c("Heading")
CoursePlot$Heading[1] <- bestSingleShipLoc$Origin-BearingSectors[1]+BearingShift


#This plot show all the delta Obs of surrounding cells
plot4<-g+
  geom_point(data = SingleShipSurround, aes(x = Longitude, y = Latitude, color = DeltaConc), alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = cm.colors(12)) +
  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 3, color = "red") +
  geom_point(data = filterShipData, aes(x = Longitude, y = Latitude)) 

# This plot is a polar sectoral viwe
plot5 <-ggplot(RadialPlotData, aes(x = PolarSector, y = DistanceSector, fill = meanDeltaConc)) +
  geom_tile() +
  coord_polar(start= BearingSectors[1]*pi/180) +
  scale_x_continuous(breaks = seq(0,360,90)) +
  scale_fill_gradient2(low = "green"
                       , mid = "yellow"
                       , high = "red") +
  geom_vline(data = CoursePlot, aes(xintercept = Heading), size = 1.5)
  
plot_grid(plot4,plot5, labels = "AUTO")
#at some point use facet_grid() for plot per pass/ship

#clean up the block
rm(BearingSectors, BearingShift, dif, DistanceSectors, matches, neares, origin, refConc, step)


rm(temp)
