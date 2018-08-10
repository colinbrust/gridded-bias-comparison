library(ggplot2)
library(magrittr)

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')

  dat <- dplyr::as.tbl(data.frame(coords, dat))

  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}

point_data <- "./analysis/data/raw_data/shapefiles/Points_Clip_Example.shp" %>%
  sf::read_sf()

ex_data <- "./analysis/data/derived_data/TopoWx/tmax/Normal/Annual/01_TopoWx_Annual_tmax_Normal.tif" %>%
  raster::raster() %>%
  raster::projectRaster(crs = sf::st_crs(point_data)[[2]]) %>%
  raster::crop(raster::extent(257983.3, 277325.6, 289178.2, 304998.5)) %>%
  gplot_data()

ggplot() +
  geom_tile(data = ex_data, aes(x = x, y = y, fill = value))  +
  geom_sf(data = point_data)


test <- ggmap::get_map(location = "Missoula, Montana",
                       source = "google",
                       maptype = "roadmap",
                       crop = FALSE,
                       zoom = 8)

ggmap::ggmap(test) +
  #geom_tile(data = ex_data, aes(x = x, y = y, fill = value))  +
  geom_sf(data = point_data)
