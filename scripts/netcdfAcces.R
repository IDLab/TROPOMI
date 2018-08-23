library (ggmap)
library (RANN)

source("scripts/1_readTropomiData.R")

filedir <- c('./bronnen')
Filenames <- list.files(path = filedir)

#define the dataframe
NO2Obs <- data.frame(longitude = as.integer(),
                         latitude = as.integer(),
                         time = as.character(),
                         Obs = as.integer(),
                         stringsAsFactors = FALSE)

#Loop through all available files
for (i in 1:length(Filenames)) {
singleNO2Obs <- fReadTropomiData(Filenames[i])
mean(singleNO2Obs$longitude)
NO2Obs <- rbind(NO2Obs,singleNO2Obs)
}

#clean up
rm (singleNO2Obs)

#remove NAs
NO2Obs <- NO2Obs[!is.na(NO2Obs$Obs),]

#with nn2 it should be possible to extract nearest neighbours
#https://www.rdocumentation.org/packages/RANN/versions/2.6/topics/nn2

#get a map from google
lon = mean(NO2Obs$longitude)
lat = mean(NO2Obs$latitude)
oceanmap <- get_map(location = c(lon, lat),
                    zoom =8)
g <- ggmap(oceanmap)

#add Obs to the map
g+
  geom_point(data = NO2Obs, aes(x = longitude, y = latitude, color = Obs), alpha = 0.6) +
  scale_color_gradientn(colours = terrain.colors(7))

