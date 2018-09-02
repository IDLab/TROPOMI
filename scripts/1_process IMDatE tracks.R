
#grab the filenames in location
Datafilenames <- list.files(path = "./Data/IMDatE_TrackFiles/.")
Datafilenames  <- paste0("./Data/IMDatE_TrackFiles/", Datafilenames)
Datafilenames <- Datafilenames[1:(length(Datafilenames)-1)]

#loop through filenames and append into single set
for (i in 1:length(Datafilenames)) {
  tempfile <- read.csv(Datafilenames[i], sep = ",", header = FALSE, stringsAsFactors = FALSE)
  tempfile[7] <- list (NULL)
  #identify lines where new ship info starts, also identify column containing IMO number
  lines <- which(tempfile == 'IMO', arr.ind = TRUE)
  
  #loop through lines to grab AIS data for each ship
  numberShipsinFile <- dim(lines)[1]
  for (j in 1:numberShipsinFile) {
    startline <- lines[j,1]+3
    if (j != numberShipsinFile) {
      endline <- lines[j+1,1]-1
      tempShipAISData <-tempfile[startline:endline,] 
    } else {
      endline <- dim(tempfile)[1]
      tempShipAISData <-tempfile[startline:endline,] 
    }
    #build a column of ship IMO numbers and names matching available positions, also extract and process columns
    tempShipIMO <- rep(as.character(tempfile[lines[j,1]+1,lines[j,2]]), dim(tempShipAISData)[1])
    tempShipName <- rep(as.character(tempfile[lines[j,1]+1,4]), dim(tempShipAISData)[1])
    tempDateTime <- tempShipAISData[,1]
    
    tempShipAISData <- cbind(tempShipAISData, tempShipIMO, tempShipName)
    #append each tempShipAISData for each loop
    ifelse (j == 1, tempfileAISData <- tempShipAISData, tempfileAISData<- rbind(tempfileAISData,tempShipAISData))
    rm(endline,startline, tempShipIMO,tempShipName, tempShipAISData)
  }
  #append each tempfileAISData for each loop
  ifelse (i == 1, AISData <- tempfileAISData, AISData<- rbind(AISData,tempfileAISData))
  rm(j, lines, numberShipsinFile, tempfile,tempfileAISData)
}
rm (i, Datafilenames)
#convert to correct formats and provide headers
colnames(AISData) <-c("TimeStamp", "Latitude", "Longitude", "Heading", "Speed", "AISSource", "IMO", "Name")
AISData$TimeStamp <- as.POSIXct(AISData$TimeStamp,tz = "UTC")

filename <-paste0('AISData_',as.character(format(Sys.time(), "%Y_%m_%d_%H_%M_%S")),'.csv')
write.csv(AISData, file = filename, row.names=FALSE)
