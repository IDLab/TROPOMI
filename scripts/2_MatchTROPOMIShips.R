#This script will read both satellite and shipdata, process into individual ship maps and visualize results.

#Calling required libraries.
library (data.table)
library (ggmap)
library (RANN)
library (dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
library (geosphere)
library (parallel)


#refer to external functions
source("scripts/1_collectSingleShipObs.R")

#read processed satellite data
load ("data/NO2Obs.Rda")

##########
#load AIS data
ShipData <- read.csv2("./data/AISData_2018_09_03_23_23_58.csv", sep = ",", stringsAsFactors = FALSE)
ShipData$Latitude <- as.numeric(ShipData$Latitude)
ShipData$Longitude <- as.numeric(ShipData$Longitude)
ShipData$Heading <- as.numeric(ShipData$Heading)
ShipData$Speed <- as.numeric(ShipData$Speed)
ShipData$TimeStamp <- as.POSIXct(ShipData$TimeStamp, tz = "UTC")
ShipData$Name <- as.factor(ShipData$Name)

#sometimes there is double entries ?!
ShipData <- unique(ShipData)

###Any filters here could also be applied when building the set. rethink every nowand then if this is right location
#if heading is missing remove from data set
ShipData <- filter(ShipData, !is.na(Heading))
#if Speed is low no use doing the analysis
ShipData$Speed <- as.numeric(ShipData$Speed)
ShipData <- filter(ShipData, Speed > 5)
#remove any ship with only one AIS point as you cannot interpolate to right time
NameFreqs <- as.data.frame(table(ShipData$Name))
NameFreqs <- filter (NameFreqs, Freq > 1)
ShipData <- filter (ShipData, Name %in% NameFreqs$Var1)

rm(NameFreqs)

############
#plot all Ships in AISdata
if (FALSE) {
  lon = mean(ShipData$Longitude)
  lat = mean(ShipData$Latitude)
  oceanmap <- get_map(location = c(lon, lat), zoom =2)
  g <- ggmap(oceanmap)
  g+
    geom_point(data = ShipData, aes(x = Longitude, y = Latitude)) 
}

################
#This plot will map ships and TROPOMI observations. Allowing to zoom in on one particular ship
###########

#plot  all Obs and all ShipLocs in a single map 
if (FALSE) {
  #get a map from google
  lon = mean(NO2Obs$Longitude)
  lat = mean(NO2Obs$Latitude)
  oceanmap <- get_map(location = c(lon, lat), zoom =2)
  g <- ggmap(oceanmap)
  #build the plot
  g+
    geom_point(data = NO2Obs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 0.002) +
    scale_color_gradientn(colours = terrain.colors(7)) +
    geom_point(data = ShipData, aes(x = Longitude, y = Latitude))
}


#########
#This chunck will unify Ship locations and TROPOMI observations into sets of observations surrounding individual ships, including intepolation for time matching
###############

#Build a vector of unique ships to proces
ShipNames <- unique(ShipData$Name)

#This function will call collection of data for single ship
fCollectSingleShip <- function(i) {
  fCollectSingleShipList  <- fCollectSingleShipObs(NO2Obs, ShipData, ShipNames[i])
  return(fCollectSingleShipList)
}

#for original saplly purposes
if (FALSE) {
  #call the function for specified number of Ships
  ShipObsList <-sapply (1:16,fCollectSingleShip)
  #restructure list into dataframes
  index <- seq(1, length (ShipObsList), by =2)
  SingleShipSurround <- rbindlist(ShipObsList[index], use.names=TRUE)
  index <- seq(2, length (ShipObsList), by =2)
  bestSingleShipLoc <- rbindlist(ShipObsList[index], use.names=TRUE) 
  rm(index)
}

N <- 180

#for (parallel(lapply for mac) purposes)
ifelse (Sys.info()["sysname"] == "Darwin" , ShipObsList <-mclapply (1:N,fCollectSingleShip), ShipObsList <-lapply (1:N,fCollectSingleShip))

for (i in 1:N) {
  #SingleShipSurroundalt <- ifelse (i == 1, ShipObsListalt[[i]]$SingleShipSurround, rbind(SingleShipSurround, ShipObsListalt[[i]]$SingleShipSurround))
  SingleShipSurround <- if (i == 1) {ShipObsList[[i]]$SingleShipSurround} else {rbind(SingleShipSurround, ShipObsList[[i]]$SingleShipSurround)}
  bestSingleShipLoc <- if (i == 1) {ShipObsList[[i]]$bestSingleShipLoc} else {rbind(bestSingleShipLoc, ShipObsList[[i]]$bestSingleShipLoc)}
}

#remove any ships where distance to nearest datapoint is too large
#first retrieve closest Observation to Ship
MinDist <-SingleShipSurround %>%
  group_by(.dots = c("Name")) %>%
  summarize(DistToOrg = min(DistToOrg, na.rm = TRUE))
#filter out any distance longer than given below
MinDist <- filter(MinDist, DistToOrg <30)

SingleShipSurround <- filter(SingleShipSurround, Name %in% MinDist$Name)

#remove any ship without any surrounding data points from TROPOMI
bestSingleShipLoc <- filter(bestSingleShipLoc, Name %in% unique(SingleShipSurround$Name))

rm(MinDist)

#########
#plot Obs and Locs of SingleShips
###########

lon = mean(bestSingleShipLoc$Longitude)
lat = mean(bestSingleShipLoc$Latitude)
oceanmap <- get_map(location = c(26.85, 34.93), zoom =6)
g <- ggmap(oceanmap)

plot1<-g+
  geom_point(data = NO2Obs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 0.01) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = ShipData, aes(x = Longitude, y = Latitude)) #+
#  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 5) 

##########
#Build datastructure to plot in polar, each sector has average of DeltaConc in area
#############

RadialPlotData<-SingleShipSurround %>%
  group_by(.dots= c("Name", "PolarSector","DistanceSector")) %>%
  summarize(meanDeltaConc = mean(DeltaConc, na.rm = TRUE))

#This plot show all the delta Obs of surrounding cells
plot4 <- ggplot() +
  geom_point(data = SingleShipSurround, aes(x = Longitude, y = normLatitude, color = DeltaConc),alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = cm.colors(12)) +
  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = normLatitude), size = 5, color = "red") +
  facet_wrap (~Name, nrow = 4, scales = "free") 

# This plot is a polar sectoral viwe
plot5 <-ggplot(RadialPlotData, aes(fill = meanDeltaConc, xmin =PolarSector, ymin = DistanceSector, xmax = PolarSector+18, ymax = DistanceSector+1)) +
  geom_rect() +
  #coord_polar(start = bestSingleShipLoc$PlotBearing*pi/180) +
  coord_polar() +
  facet_wrap (~Name,nrow = 4, scales = NULL) +
  scale_x_continuous(labels = NULL) +
  scale_fill_gradient2(low = "green"
                       , mid = "yellow"
                       , high = "red") +
  geom_vline(data = bestSingleShipLoc, aes(xintercept = AdjOrigin), size = 1.5)

#plot_grid(plot4,plot5, labels = "AUTO", ncol =1)



