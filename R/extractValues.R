# rasters is the list of rasters from the makeImages function

# this function takes a list of rasters, extracts their values to points,
# and returns a list of data frames

shpNaRm <- function(shp) {

  shp$CD[is.na(shp$CD)] <- "Out of State"
  return(shp)
}

ptFile <-
  "./analysis/data/raw_data/shapefiles/mtPtsCDs.shp" %>%
  sf::read_sf() %>%
  shpNaRm()

extractValues <- function(rastList, ptFile) {

  # library(raster)
  # library(tibble)
  # library(magrittr)
  # library(stringr)
  # library(sf)
  # library(velox)

  ptExtract <- function(rastImg) {


    sourceName <- stringr::str_replace(basename(raster::filename(rastImg)), ".tif","")

    print(sourceName)

    reproj <- sf::st_transform(ptFile, raster::projection(rastImg))

    vx <- velox::velox(rastImg)

    vx$extract_points(sp = reproj) %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(c(sourceName))

  }

  lapply(rastList,ptExtract)


}
