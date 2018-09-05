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

################
#This part of code will map ships and TROPOMI observations. Allowing to zoom in on one particular ship
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

#Build a vector of unique ships to proces
ShipNames <- unique(ShipData$Name)
  
fCollectSingleShip <- function(i) {
  fCollectSingleShipList  <- fCollectSingleShipObs(NO2Obs, ShipData, ShipNames[i])
  return(fCollectSingleShipList)
}

ShipObsList <-sapply (7:8,fCollectSingleShip)

index <- c(1,3)
test3<- do.call(rbind, ShipObsList[index])
test3<- rbindlist(ShipObsList[index], use.names=TRUE) #supposedly the faster one


for (i in 1:length(ShipObsList)) {
  i %% 2 == 0
  if (i==2) {ShipSurround <- as.data.frame(ShipObsList[i])}
  if (i==4) {ShipSurround <- rbind(ShipSurround, as.data.frame(ShipObsList[i]))}
}

test2<-do.call(rbind.data.frame, test)
unlist(test, recursive = FALSE)
str(test)
length(test)

################
#Call function to get individual ship data
fCollectSingleShipObsList <- fCollectSingleShipObs(NO2Obs, ShipData, ShipNames[8])
SingleShipSurround <- fCollectSingleShipObsList$SingleShipSurround
bestSingleShipLoc <- fCollectSingleShipObsList$bestSingleShipLoc


#functie definieren die per schipcombi de juiste data over onderlinge afstand verzameld
fMinAfstand <- function (i) {
  #selecteer alleen de waardes voor een specifiek paar
  selectie_i <- afstanden_9d_combis %>% filter(MMSI_combi == unique(afstanden_9d_combis$MMSI_combi)[i])
  #filter gevonden waardes op minimum plus de waardes die binnen de opgegeven bandbreedte liggen
  min_afstand_tijd_i <-
    selectie_i %>%
    filter(Distance <= (1 + bandbreedte) * selectie_i[which.min(selectie_i$Distance),7])
  return(min_afstand_tijd_i)
}

#standaard lapply call
if (FALSE) {
  list_min_afstand_tijd<-lapply (1:length(unique(afstanden_9d_combis$Distance)), fMinAfstand)
}

#########
#plot Obs and Locs of SingleShipSingleShipLoc <- filter(ShipData, Name == "LEOPOLD OLDENDORFF")

lon = mean(bestSingleShipLoc$Longitude)
lat = mean(bestSingleShipLoc$Latitude)
oceanmap <- get_map(location = c(lon, lat), zoom =9)
g <- ggmap(oceanmap)

plot1<-g+
  geom_point(data = NO2Obs, aes(x = Longitude, y = Latitude, color = Obs), alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = terrain.colors(7)) +
  geom_point(data = SingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name)) +
  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 5) 

##########
#Build datastructure to plot in polar, each sector has average of DeltaConc in area
RadialPlotData<-SingleShipSurround %>%
  group_by(.dots= c("PolarSector","DistanceSector")) %>%
  summarize(meanDeltaConc = mean(DeltaConc, na.rm = TRUE))

#This plot show all the delta Obs of surrounding cells
plot4<-g+
  geom_point(data = SingleShipSurround, aes(x = Longitude, y = Latitude, color = DeltaConc), alpha = 0.6, shape = 15, size = 6) +
  scale_color_gradientn(colours = cm.colors(12)) +
  geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 5, color = "red") +
  geom_point(data = ShipData, aes(x = Longitude, y = Latitude)) 

# This plot is a polar sectoral viwe
plot5 <-ggplot(RadialPlotData, aes(x = PolarSector, y = DistanceSector, fill = meanDeltaConc)) +
  geom_tile() +
  coord_polar(start= bestSingleShipLoc$PlotBearing*pi/180) +
  scale_x_continuous(breaks = seq(0,360,90)) +
  scale_fill_gradient2(low = "green"
                       , mid = "yellow"
                       , high = "red") +
  geom_vline(data = bestSingleShipLoc, aes(xintercept = AdjOrigin), size = 1.5)
  
plot_grid(plot4,plot5, labels = "AUTO")
#at some point use facet_grid() for plot per pass/ship


