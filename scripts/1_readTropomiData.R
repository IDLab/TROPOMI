library (ncdf4)
library (lubridate)

fReadTropomiData <- function (filename) {
  PathFile <- paste0("./bronnen/", filename)
  rawdata<-nc_open(PathFile)
  
  #extract tropocolumn
  #rawdata$var[[6]]$name
  temp <-rawdata$var[[6]]
  tropo_column<-ncvar_get(rawdata, temp)
  
  #extract Latitude
  #rawdata$var[[1]]$name
  temp <-rawdata$var[[1]]
  Latitude<-ncvar_get(rawdata, temp)
  
  #extract Longitude
  #rawdata$var[[2]]$name
  temp <-rawdata$var[[2]]
  Longitude<-ncvar_get(rawdata, temp)
  
  #extract delta_time
  #THIS IS A MESS, documentation talks about adding delta_time in seconds. If one applies this you get date in 2019...
  #rawdata$var[[3]]$name
  temp <-rawdata$var[[3]]
  delta_time<-ncvar_get(rawdata, temp)
  
  #extract time
  #rawdata$var[[4]]$name
  temp <-rawdata$var[[4]]
  time<-ncvar_get(rawdata, temp)
  time <- parse_date_time(time, orders = "ymd HMS")
  
  #define the dataframe
  fileNO2Obs <- data.frame(Latitude = as.integer(),
                           Longitude = as.integer(),
                           time = as.character(),
                           Obs = as.integer(),
                           stringsAsFactors = FALSE)
  
  #Function to create a list with all wanted data
  fGrabObs <- function (i) {
    tempNO2Obs <- data.frame(Latitude = rep(as.integer(NA), length(time)),
                             Longitude = rep(as.integer(NA), length(time)),
                             time = rep(as.character(NA), length(time)),
                             Obs = rep(as.integer(NA), length(time)),
                             stringsAsFactors = FALSE)
    
    tempNO2Obs$Longitude <- Longitude[i,]
    tempNO2Obs$Latitude <- Latitude[i,]
    tempNO2Obs$time <- time
    tempNO2Obs$Obs <- tropo_column[i,]
    return(tempNO2Obs)
  }
  
  #call function to create List with wanted data
  listObs <- lapply (1:dim(Longitude)[1], fGrabObs)
  #from list to df
  fileNO2Obs<-do.call(rbind, listObs)
  
  return (fileNO2Obs)
}

