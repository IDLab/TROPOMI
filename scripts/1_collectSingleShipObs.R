library(geosphere)
library (RANN)
library (dplyr)

fCollectSingleShipObs <- function (NO2Obs, Shipdata, ShipName) {
  #ShipName <- ShipNames[1]
  
  
  SingleShipLoc <- filter(ShipData, Name == ShipName)
  nrow(SingleShipLoc)
 # if (nrow(SingleShipLoc)>1) {
    #extract nearest neighbours observations of known AIS locations
    matches <- nn2(NO2Obs[,1:2], SingleShipLoc[,2:3],1)
    #combine Obs en Loc
    SingleShipMatches <- cbind (SingleShipLoc,NO2Obs[matches$nn.idx,])
    #Calculate differene in Time of datapoint
    SingleShipMatches$DeltaT <- SingleShipMatches[,1]-SingleShipMatches[,11]
    
    # Select the rows with the lowest timedifference
    best <- which.min(abs(SingleShipMatches$DeltaT))
    nextbest <- nn2(SingleShipMatches$DeltaT, SingleShipMatches$DeltaT[best],2)$nn.idx[2]
    
    #build a df on the best available match for ship location
    bestSingleShipLoc <- filter (SingleShipLoc, TimeStamp == SingleShipMatches$TimeStamp[best])
    
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
    matches <- nn2(NO2Obs[,1:2], bestSingleShipLoc[,2:3],100)
    SingleShipSurround <- NO2Obs[matches$nn.idx,]
    SingleShipSurround$DistToOrg <- distGeo(cbind(SingleShipSurround$Longitude,SingleShipSurround$Latitude), c(bestSingleShipLoc$Longitude, bestSingleShipLoc$Latitude))/1000
    #determine for each point the difference in Obs conc with the cell where the ship is at >0 means higher than where ship is. 
    refConc <-  SingleShipSurround$Obs[which(SingleShipSurround$DistToOrg == min(SingleShipSurround$DistToOrg))]
    SingleShipSurround$DeltaConc <- SingleShipSurround$Obs-refConc
    # calculate the bearing of each cell surrounding the ship, to later assign sector in polar plot
    origin <-c(bestSingleShipLoc$Longitude,bestSingleShipLoc$Latitude)
    SingleShipSurround$Bearing <- sapply(1:nrow(SingleShipSurround), function (x) {bearingRhumb(origin,c(SingleShipSurround$Longitude[x],SingleShipSurround$Latitude[x]))})
    
    ##assign Polar sector and Distance sector to the Obs surrounding the ship
    #YOU MUST USE AN EVEN NUMBER OF SECTORS
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
    
    #Add a name to which the Obs is attached
    SingleShipSurround$Name <- bestSingleShipLoc$Name
    
    #Normalize on Latitude to facilitate plotting later on
    bestSingleShipLoc$normLatitude <- bestSingleShipLoc$Latitude-bestSingleShipLoc$Latitude
    SingleShipSurround$normLatitude <- SingleShipSurround$Latitude - bestSingleShipLoc$Latitude
    
    #To facilitate plotting of actual course in polar plot, it is necessary to construct an adjusted origin course as plot orientation is adjusted to center sector on origin.
    # also add a Bearing on shift of first sector from North as true course of sector
    #bestSingleShipLoc$AdjOrigin <- bestSingleShipLoc$Origin-BearingSectors[1]+BearingShift
    bestSingleShipLoc$AdjOrigin <- bestSingleShipLoc$Origin
    bestSingleShipLoc$PlotBearing <- BearingSectors[1]
    
    #Plot results of ship location interpolation and NO2Obs
    if (FALSE) {
      lon = mean(bestSingleShipLoc$Longitude)
      lat = mean(bestSingleShipLoc$Latitude)
      oceanmap <- get_map(location = c(lon, lat), zoom =9)
      g <- ggmap(oceanmap)
      g+
        geom_point(data = SingleShipSurround, aes(x = Longitude, y = Latitude, color = DistToOrg), alpha = 0.6, shape = 15, size = 6) +
        scale_color_gradientn(colours = terrain.colors(7)) +
        geom_point(data = SingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name)) +
        geom_point(data = bestSingleShipLoc, aes(x = Longitude, y = Latitude, shape = Name), size = 5) 
    }
 # }
  #remove datapoints that are too far away
  SingleShipSurround<-filter(SingleShipSurround, DistToOrg < 100)
  
  #transfer two DFs with results into single list for return from funciton
  fCollectSingleShipObsList <- list("SingleShipSurround" = SingleShipSurround, "bestSingleShipLoc" = bestSingleShipLoc)
  return (fCollectSingleShipObsList)
  
}
