# rasters is the list of rasters from the makeImages function

# this function takes a list of rasters, extracts their values to points,
# and returns a list of data frames

extractValues <- function(rastList) {

  library(raster)

  shpNaRm <- function(shp) {

    shp$CD[is.na(shp$CD)] <- "Out of State"
    return(shp)
  }

  mtPoints <-
    "./analysis/data/raw_data/shapefiles/mtPtsCDs.shp" %>%
    raster::shapefile() %>%
    shpNaRm()

  ptExtract <- function(rastImg) {

    sourceName <- stringr::str_replace(basename(raster::filename(rastImg)), ".tif","")

    reproj <- sp::spTransform(mtPoints, raster::crs(rastImg))

    vx <- velox::velox(rastImg)

    df <- data.frame(vx$extract_points(sp = reproj))

    colnames(df) <- c(sourceName)

    return(df)

  }

  return(lapply(rastList,ptExtract))


}

# byDiv <- function(shp) {
#
#   shp <- shp[shp$CD == CD,]
#
# }

#test$CD <- reproj@data$CD
