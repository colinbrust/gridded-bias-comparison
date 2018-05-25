
hexFile <-
  "./analysis/data/raw_data/shapefiles/frontHex.shp" %>%
  sf::read_sf() %>%
  dplyr::mutate("PointID" = as.character(ORIG_FID))

makeSingleMap <- function(location = "front", timeFilter = "01") {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")

  #### DELETE COMMENT AFTER YOU MAKE MAPTITLE FUNCTION ####
  mapTitle <- makeTitles(variable, time, stat, dataset)

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::right_join(hexFile) %>%
    dplyr::select(-GRID_ID, -ORIG_FID, -CD) %>%
    dplyr::filter(Index == timeFilter, Dataset != "Ensemble") %>%
    #factorData(time) %>%
    sf::st_sf()

  ggplot() +
    geom_sf(data = dat, aes(fill = Value)) +
    facet_wrap(~Dataset) +
    scale_fill_gradient2(low = "#4f63dd", high = "#ff0101")


}

fill <-
  dat %>%
  dplyr::filter(Dataset == "Ensemble") %>%
  dplyr::select(Value) %>%
  rep(nrow(dat)/nrow(dplyr::filter(dat, Dataset == "Ensemble"))) %>%
  unlist() %>%
  unname()

return(tibble::add_column(dat, "EnsVal" = fill))

