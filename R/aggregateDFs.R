# dfList is a list of data frames created from extractValues.R
# this function takes a list of data frames and returns a large data frame

shpNaRm <- function(shp) {

  shp$CD[is.na(shp$CD)] <- "Out of State"
  return(shp)
}

ptFile <-
  "./analysis/data/raw_data/shapefiles/mtPtsCDs.shp" %>%
  raster::shapefile() %>%
  shpNaRm()

aggregateDFs <- function(dfList, ptFile) {

  # library(tibble)
  # library(tidyr)
  # library(dplyr)

  dat <-  do.call("cbind", dfList) %>%
          tibble::as_data_frame() %>%
          tibble::add_column(ptFile@data$CD) %>%
          tibble::add_column(ptFile@data$ORIG_FID)%>%
          dplyr::rename("ClimateDivision" = "ptFile@data$CD")%>%
          dplyr::rename("PointID" = "ptFile@data$ORIG_FID") %>%
          tidyr::gather(key = "Names",
                        value = "Value",
                        -"ClimateDivision",
                        -"PointID") %>%
          tidyr::separate(col = "Names",
                          sep = "_",
                          into = c("Index", "Dataset", "Time", "Variable", "Statistic"))

  if(dat$Variable[1] == "tmin" || dat$Variable[1] == "tmax") {

    dat$Value[dat$Dataset == "Gridmet"] = dat$Value[dat$Dataset == "Gridmet"] - 273.15
  }

  return(dat)
}
