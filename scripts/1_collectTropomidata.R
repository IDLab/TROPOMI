# This script will load, filter, aggregate and save to disk all available TROPOMI files in single variable.

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

save(NO2Obs, file = "data/NO2Obs.Rda")
