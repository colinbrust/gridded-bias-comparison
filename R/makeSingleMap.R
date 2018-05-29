
hexFile <-
  "./analysis/data/raw_data/shapefiles/frontHex.shp" %>%
  sf::read_sf() %>%
  dplyr::mutate("PointID" = as.character(ORIG_FID))

hexFile <-
  "./analysis/data/raw_data/shapefiles/hexAttributed.shp" %>%
  sf::read_sf() %>%
  dplyr::mutate("PointID" = as.character(ORIG_FID))


makeSingleMap <- function(location = "front", timeFilter = "01") {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")
  source("./R/mapTitles.R")

  #### DELETE COMMENT AFTER YOU MAKE MAPTITLE FUNCTION ####
  mapTitle <- mapTitles(variable, time, stat, c()) #c(...))

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::right_join(hexFile) %>%
    dplyr::select(-GRID_ID, -ORIG_FID, -CD) %>%
    dplyr::filter(Index == timeFilter, Dataset != "Ensemble") %>%
    #factorData(time) %>%
    sf::st_sf() %>%
    sf::st_transform(4326)

  b <- sf::st_bbox(hexFile)

ggplot() +
    geom_sf(data = dat, aes(fill = Value), color = NA) +
    facet_wrap(~Dataset) +
    scale_fill_continuous(low = "blue", high = "orange")


}

test <- G +
  geom_sf(data = dat, aes(fill = Value), color = NA) +
  facet_wrap(~Dataset) +
  scale_fill_continuous(low = "blue", high = "orange")

bMap <- function(g) {

  g %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>%
    unname %>%
    ggmap::get_map() %>%
    ggmap::ggmap()
}

