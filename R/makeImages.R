# fileList is a list of vectors created from the getPaths function.

# This function will take the file list and create lists of rasters.
makeImages <- function(fileList) {

  listToRaster <- function(fileVec) {

    lapply(fileVec, raster::raster)

  }

  lapply(fileList, listToRaster)

}
