# This function plots a map corresponding to the user's inputs

# Variable   - Either "tmin", "tmax" or "ppt"
# Time       - Either "Monthly", "Seasonal" or "Annual"
# Stat       - Either "Normal" or "SD"
# timeFilter - Which subset of your time variable you want to map. For Monthly
             # choose between "01" - "12", corresponding to Jan - Dec. For
             # Seasonal, choose between "01" - "04", corresponding to Winter -
             # Autumn
# dev        - TRUE if you want to map each dataset's deviation from the
             # average of all datasets. FALSE if you want to just map the raw
             # data values for each datset.
# hexFile    # The sf object that is used for mapping. Use:
             # "./analysis/data/raw_data/shapefiles/hexFile.shp" %>%
                  # sf::read_sf() %>%
                  # dplyr::mutate("PointID" = as.character(ORIG_FID))

hexFile <-
  "./analysis/data/raw_data/shapefiles/hexFile.shp" %>%
  sf::read_sf() %>%
  dplyr::mutate("PointID" = as.character(ORIG_FID))


make_map <-function(variable, time, stat, timeFilter = "01", dev = FALSE,
                    hexFile) {
    # library(feather)
    # library(rlang)
    library(ggplot2)
    library(mcor)
    source("./R/viz_map.R")
    source("./R/titles_map.R")

    if (dev) {
      Value <-  rlang::sym("EnsDiff")
    } else {
      Value <-  rlang::sym("Value")
    }

    dat <- "./analysis/data/derived_data/Extracts/" %>%
      paste0(time) %>%
      paste(variable, paste0(stat, ".feather"), sep = "_") %>%
      feather::read_feather() %>%
      dplyr::right_join(hexFile) %>%
      dplyr::filter(Index == timeFilter, Dataset != "Ensemble",
                    Montana == "yes") %>%
      sf::st_sf() %>%
      dplyr::mutate(Value = round(!!Value)) %>%
      dplyr::select(Dataset, Value) %>%
      dplyr::group_by(Dataset, Value) %>%
      dplyr::summarise()


    ggplot() +
      geom_sf(data = dat, aes(fill = Value), color = NA) +
      geom_sf(
        data = mt_counties_simple,
        fill = NA,
        color = "gray40",
        size = 0.5,
        alpha = 0.1
      ) +
      geom_sf(
        data = mt_state_simple,
        fill = NA,
        color = "gray40",
        size = 1
      ) +
      labs(title = titles_map(variable, time, stat, timeFilter, dev)) +
      mdt_theme_map() +
      pal(dev, variable) +
      facet_wrap( ~ Dataset)

  }
