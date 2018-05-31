
hexFile <-
  "./analysis/data/raw_data/shapefiles/hexFile.shp" %>%
  sf::read_sf() %>%
  dplyr::mutate("PointID" = as.character(ORIG_FID))


make_map <- function(variable, time, stat, timeFilter = "01", ...) {

  #library(feather)
  library(ggplot2)
  library(mcor)
  source("./R/map_viz.R")
  # source("./R/factor_data.R")
  source("./R/map_titles.R")

  mapTitle <- map_titles(variable, time, stat, timeFilter, c(...))

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time) %>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::right_join(hexFile) %>%
    dplyr::filter(Index == timeFilter, Dataset != "Ensemble",
                  Montana == "yes") %>%
    # dplyr::filter_(...) %>%
    sf::st_sf()%>%
    dplyr::mutate(Value = round(Value)) %>%
    dplyr::select(Dataset, Value) %>%
    dplyr::group_by(Dataset, Value) %>%
    dplyr::summarise()


  ggplot() +
    geom_sf(data = dat, aes(fill = Value), color = NA) +
    geom_sf(data = mt_counties_simple,
            fill = NA,
            color = "gray40",
            size = 0.5,
            alpha = 0.1) +
    geom_sf(data = mt_state_simple,
            fill = NA,
            color = "gray40",
            size = 1) +
    labs(title = mapTitle[1], subtitle = mapTitle[2]) +
    mdt_theme_map() +
    scale_fill_distiller(name = mapTitle[3], palette = mapTitle[4]) +
    facet_wrap(~Dataset)
}

