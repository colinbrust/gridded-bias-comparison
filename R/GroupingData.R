# Necessary libraries
library(raster)
library(stringr)
library(velox)
library(rgdal)



#### Create filepath variables and create shapefiles ####
# Directory where datasets are located
imgPath <- "Y:/Data/"

# Where final plots will be saved
figOut  <- "Y:/Users/colin.brust/Gridded_Met/Figs"

# Create pointfile variables for the two coordinate systems being used
GCSpoints <-   raster::shapefile("Y:/Projects/MCO_Gridded_Met_Eval/Code/Points_Shapefile/GCS_Hex_Pts.shp")
PRISMpoints <- raster::shapefile("Y:/Projects/MCO_Gridded_Met_Eval/Code/Points_Shapefile/PRISM_Hex_Pts.shp")

# Create hexagon variables for the two coordinate systems being used
GCSHex <-   raster::shapefile("Y:/Projects/MCO_Gridded_Met_Eval/Code/Hex_Files/GCS_Hexes_Correct.shp")
PRISMHex <- raster::shapefile("Y:/Projects/MCO_Gridded_Met_Eval/Code/Hex_Files/PRISM_HEXES.shp")

# Vectors containing names of GEE datasets and other datasets
allSets <- c("CHIRPS", "Gridmet", "Daymet", "TopoWx", "PRISM")
monList <- c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December")
seasList <- c("Winter", "Spring", "Summer", "Autumn")

#### Create list of image paths and ensure that all paths exist ####

# This is a function that will take a vector containing the dataset names
# to be used and return the filepaths to those datasets.
# INPUTS - a vector containing the datasets to be analyzed. Options include:
#          "PRISM", "Gridmet", "Daymet", "TopoWx", and "Chirps"
getPaths <- function(InputDatasets) {

  imgPaths <- c()
  for (i in 1:length(InputDatasets)) {
    imgPaths[i] <- paste0(imgPath, InputDatasets[i], "/", "Extracts/", "Brust")}
  for (i in 1:length(imgPaths)) {
    if (file.exists(imgPaths[i]) == FALSE)
    {return(print(paste("The Provided Datasets were either input incorrectly +
                        or do not exist. Please check the datasets list you +
                        used as an input")))}
  }

  return(imgPaths)
}



#### Determine files to select given the period ####

# This is a function that will return each possible image path to be used
# in the comparison.
# INPUTS -
    # variable - Choose from: "ppt", "tmax", "tmin"

    # metric - Choose from: "Normal" or "SD"
    # period - Choose from: "Annual", "Monthly", or "Seasonal"
    # Creates the list for annual timestep
allFiles <- function(metric, variable, period, datasetPaths) {

  # Creates a vector that will contain all of the new image filepaths
  newPaths <- list()
  for (i in 1:length(datasetPaths)) {
    vecvec <- c()
    pathName <- list.files(paste0(datasetPaths[i],"/", variable, "/", metric, "/", period),
                           recursive = T, full.names = T)
    vecvec <- append(vecvec, pathName)
    newPaths[[i]] <- vecvec
  }

  return(newPaths)
}



#### Determine which images came from Google Earth Engine ####

# a function that will return a vector containing a T or F for whether each
# filepath returned in allFiles is from GEE
# INPUTS -
    # The vector of filepaths returned from allFiles
isGEE <- function(fileVec) {
  boolVec <- c()
  for (i in 1:length(fileVec)) {
    if ((grepl("Gridmet", fileVec[i]) == T) | (grepl("CHIRPS", fileVec[i]) == T) |
        (grepl("Daymet", fileVec[i]) == T)) {
    boolVec[i] <- 1
    } else {boolVec[i] <- 0}
  }

  return(boolVec)
}


#### Convert filepaths into raster images ####

# function that will take the output of the allFiles function and convert all
# filepaths into raster images
# INPUTS -
    # Period - A vector containing which specific months or season to use. Refer to months
    # as 1, 2, 3, 4... and seasons as 1, 2, 3, 4 for winter, spring, summer, and
    # autumn respectively. If annuals are being compared, just enter "Annual".
    # ex. c("Monthly", 1, 6, 8) for January, June and August or
    # c("Seasonal", 2) for spring

    # pathNames - The pathnames vector that was made using the allFiles function

    # boolVec - The boolean vector that was made using the isGEE function

MandSRasters <- function(period, pathNames, boolVec){

  # Empty vector that all rasters will be appended to.
  rastList <- list()

  # Other vectors that will be filled throughout the function
  pathIndex <- c()
  finalPaths <- c()

  for (i in 2:length(period)) {
    pathIndex[i-1] <- as.numeric(period[i])
  }

  # For every dataset that will be evaluated...
  for (i in 1:length(pathNames)) {

    vecvecs <- c()

    # If the image wasn't created in GEE...
    if(boolVec[i] == 0){

      # Create raster images for each specified month
      for (j in 1:length(pathIndex)){
        vecvecs <- append(vecvecs, raster(pathNames[[i]][pathIndex[j]]))
      }

    # If it is a GEE image...
    } else if (boolVec[i] == 1) {

      for (j in 1:length(pathIndex)){

        # Create raster images for each month according to the corresponding band
        vecvecs <- append(vecvecs, raster(pathNames[[i]][1], band = pathIndex[j]))
      }
    }

    # append the vector of raster images to the raster list
    rastList[[i]] <- vecvecs
  }

  return(rastList)
}

annualRasters <- function(period, pathNames, boolVec){

  # vectors that will be filled throughout the function
  rastList <- list()
  pathIndex <- c()
  finalPaths <- c()


  for (i in 1:length(pathNames)) {

    vecvecs <- c()

    vecvecs <- append(vecvecs, raster(pathNames[[i]]))
    rastList[[i]] <- vecvecs
  }

  return(rastList)
}



#### Give hexagons/points the underlying raster values ####

# A function that will be used in veloxExtract to calculate polygon means
meanFun <- function(x){mean(x, na.rm = T)}

# uses the velox library to extract raster cell values
hexExtract <- function(rasterData) {

  vx <- velox(rasterData)
  # if it is a prism dataset, use the prism projected points
  if (grepl(pattern = "PRISM", x = rasterData@file@name) == 1) {

    hexCopy <- PRISMHex
    newPts <- vx$extract(sp = PRISMHex, df = T, small = T, fun = meanFun)
    hexCopy@data$Values <- newPts[,2]
    hexCopy@data$name <- rasterData@file@name

  } else {

    hexCopy <- GCSHex
    newPts <- vx$extract(sp = GCSHex, df = T, small = T, fun = meanFun)
    hexCopy@data$Values <- newPts[,2]
    hexCopy@data$name <- rasterData@file@name

  }

  return(hexCopy)

}

pointExtract <- function(rasterData) {

  vx <- velox(rasterData)

  # if it is a prism dataset, use the prism projected points
  if (grepl(pattern = "PRISM", x = rasterData@file@name) == 1) {

    ptsCopy <- PRISMpoints
    newPts <- vx$extract_points(sp = PRISMpoints)
    ptsCopy@data$Values = newPts
    ptsCopy@data$Name = rasterData@file@name

  } else {

    ptsCopy <- GCSpoints
    newPts <- vx$extract_points(sp = GCSpoints)
    ptsCopy@data$Values = newPts
    ptsCopy@data$Name = rasterData@file@name

  }

  return(ptsCopy)

}

####  Master Functions That Aggregate All Other Functions and Return Data####
# INPUTS -
          # - datasetsToUse: A vector containing the names of the datasets that
          #   will be evaluated
          # - variable: either "tmin" "tmax" or "ppt"
          # - metric: Either "Normal" or "SD"
          # - period: A vector starting with either "Annual", "Seasonal" or "Monthly"
          #   then followed by the desired month or season numbers
          # ptHex = True for points, False for Hexagon

createNames <- function(datasetsToUse, variable, metric, period) {

  nameList <- c()

  if (period[1] == "Monthly") {

    periodList <- monList

    for(i in 1:(length(datasetsToUse))) {

      for (j in 1:(length(period) -1)) {

        index <- ((i-1)*(length(period)-1)) + j
        monIndex <- as.numeric(period[j+1])

        nameList[index] <- paste(datasetsToUse[i], periodList[monIndex], variable, metric, sep = " ")

      }

    }

  } else if (period[1] == "Seasonal") {

    periodList <- seasList

    for(i in 1:(length(datasetsToUse))) {

      for (j in 1:(length(period) -1)) {

        index <- ((i-1)*(length(period)-1)) + j
        monIndex <- as.numeric(period[j+1])

        nameList[index] <- paste(datasetsToUse[i], periodList[monIndex], variable, metric, sep = " ")


      }

    }

  } else if (period[1] == "Annual") {


    for(i in 1:(length(datasetsToUse))) {


        nameList[i] <- paste(datasetsToUse[i], "Annual", variable, metric, sep = " ")


      }


  }

  return(nameList)
}

makePtFiles <- function(datasetsToUse, variable, metric, period) {

  # A list containing all necessary paths
  dataUse <- allFiles(metric, variable, period[1], getPaths(datasetsToUse))

  # A list of booleans that will tell whether or not each dataset is from GEE
  boolList <- isGEE(dataUse)

  # Returns a list of the desired rasters
  if (period[1] == "Seasonal" || period[1] == "Monthly"){
    rastList <- MandSRasters(period = period, pathNames = dataUse, boolVec = boolList)

  }else {
    rastList <- annualRasters(period = period, pathNames = dataUse, boolVec = boolList)
  }

  spatialList <- lapply(unlist(rasterList), pointExtract)

  return(spatialList)

}

# Same as above function only it produces a list of hexagons rather than points
makeHexFiles <- function(datasetsToUse, variable, metric, period) {

  # A list containing all necessary paths
  dataUse <- allFiles(metric, variable, period[1], getPaths(datasetsToUse))

  # A list of booleans that will tell whether or not each dataset is from GEE
  boolList <- isGEE(dataUse)

  # Returns a list of the desired rasters
  if (period[1] == "Seasonal" || period[1] == "Monthly"){
    rastList <- MandSRasters(period = period, pathNames = dataUse, boolVec = boolList)

  }else {
    rastList <- annualRasters(period = period, pathNames = dataUse, boolVec = boolList)
  }

  spatialList <- lapply(unlist(rasterList), hexExtract)

  return(spatialList)

}

makeMatrix <- function(datasetsToUse, variable, metric, period) {

  # A list containing all necessary paths
  dataUse <- allFiles(metric, variable, period[1], getPaths(datasetsToUse))

  # A list of booleans that will tell whether or not each dataset is from GEE
  boolList <- isGEE(dataUse)

  # Returns a list of the desired rasters
  if (period[1] == "Seasonal" || period[1] == "Monthly"){
    rastList <- MandSRasters(period = period, pathNames = dataUse, boolVec = boolList)

  }else {
    rastList <- annualRasters(period = period, pathNames = dataUse, boolVec = boolList)
  }

  imgNames <- createNames(datasetsToUse, variable, metric, period)
  rastList <- unlist(rastList)
  valMat <- matrix(nrow = 164206, ncol = length(rastList))

  for (i in 1:length(rastList)) {

    vx <- velox(rastList[[i]])

    if (grepl(pattern = "PRISM", x = rastList[[i]]@file@name) == 1) {

      newPts <- vx$extract_points(sp = PRISMpoints)
      valMat[,i] <- newPts

    } else {

      newPts <- vx$extract_points(sp = GCSpoints)
      valMat[,i] <- newPts

    }

    print(paste(i, " is done"))
  }

  colnames(valMat) <- imgNames

  for(i in 1:length(imgNames)) {

    if (grepl(pattern = "Gridmet", x = imgNames[i]) && (variable == "tmax" || variable == "tmin")) {

      valMat[,i] = valMat[,i] - 273.15

    }

  }

  return(valMat)


}

saveCSVs <- function(datasetsToUse, variable, metric, period) {

  print("making matrix")

  valMat <- makeMatrix(datasetsToUse, variable, metric, period)

  print("Writing csv")

  write.csv(valMat, file = paste0(figOut, period[1], metric, variable, ".csv"))


}

datasetsToUse = c("Daymet", "TopoWx", "Gridmet", "PRISM")
variable = "tmin"
metric = "SD"
period = c("Annual")

#### Save out the Shapefiles ####

saveImages <- function(spatialObj) {

  if (class(spatialObj) == "SpatialPointsDataFrame") {

    outPath = paste(figOut, "Points", variable, metric, period[1], sep = "/")


  } else if (class(spatialObj) == "SpatialPolygonsDataFrame") {

    outPath = paste(figOut, "Hex", variable, metric, period[1], sep = "/")

  }


}





#### MISC Functions and Stuff ####
# Syntax for saving out shapefiles
# writeOGR(obj = test2, dsn = outDir, layer = "pointTest2", driver = "ESRI Shapefile")

##indexing syntax
#mtPoints <-  pointFile[pointFile$Montana == "yes",]

# This function will extract raster values to points.
# At the moment it is unnecessary.

imgUse1 <- makeMasterList(c("PRISM", "CHIRPS"), "ppt", "Normal", c("Monthly", 1, 7))

PRISMIMG <- imgUse1[[1]][[1]]
ChirpsImg <- imgUse1[[2]][[1]]

PRISMExtract <- veloxExtract(PRISMIMG)
ChirpsExtract <- veloxExtract(ChirpsImg)

writeOGR(obj = ChirpsExtract, dsn = "C:/Workspace/ExampleImages", layer = "ChirpsExtract", driver = "ESRI Shapefile")
