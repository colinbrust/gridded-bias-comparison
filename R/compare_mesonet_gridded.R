compare_mesonet_gridded <- function(start_date, end_date, variable) {

  library(magrittr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(velox)
  library(ggplot2)

  # start_date = "2017-09-01"
  # end_date = "2017-10-01"
  # variable = "tmin"

  analysis_dates <- seq(lubridate::as_date(start_date),
                     lubridate::as_date(end_date),
                     by = "days") %>%
    head(-1)

  plot_title <- paste("Comparison of Gridded and Mesonet", variable, "Values")

  dat <- list.dirs("./analysis/data/raw_data/daily_data",
                             full.names = T) %>%
    tail(-1) %>%
    list.files(recursive = T, full.names = T) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames("filename") %>%
    dplyr::mutate(base = basename(filename) %>%
                    tools::file_path_sans_ext()) %>%
    tidyr::separate(base,
                    into = c("dataset", "var", "date"),
                    by = "_") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'pr' | var == 'prcp',
                                      'ppt',
                                      var)) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'tmmn',
                                      'tmin',
                                      var)) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'tmmx',
                                      'tmax',
                                      var)) %>%
    dplyr::filter(var == variable) %>%
    dplyr::group_by(filename) %>%
    dplyr::mutate(grid_values = list(extract_grid_vals(filename))) %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    tidyr::gather(key = station_name, value = grid_value, -filename, -dataset,
                  -var, -date) %>%
    dplyr::full_join(extract_mes_vals(variable)) %>%
    dplyr::filter(!is.na(dataset)) %>%
    dplyr::mutate(dataset = factor(dataset),
                  station_name = factor(station_name))

  ggplot(dat, aes(x = grid_value, y = mesonet_value, color = dataset)) +
    geom_point() +
    viz_mesonet(variable, plot_title) +
    facet_wrap(~station_name)

}

extract_grid_vals <- function(fname) {

  rast <- velox::velox(fname)

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/mesonet_sites.shp" %>%
    sf::read_sf() %>%
    sf::st_transform(fname %>% raster::raster() %>%
                       raster::projection())

 rast$extract_points(sp = mesonet_sites) %>%
    tibble::as_tibble() %>%
    tibble::add_column(sites = mesonet_sites$station) %>%
    tidyr::spread(key = sites, value = V1)

}

extract_mes_vals <- function(variable) {

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations.csv") %>%
    dplyr::rename(ppt = precipitation) %>%
    dplyr::select(days, !!variable, station_name) %>%
    dplyr::rename(value = !!variable) %>%
    dplyr::rename(date = days) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::rename(mesonet_value = value)

}

viz_mesonet <- function(variable, plot_title) {

  myColors <- c("#FFC857", "#E9724C", "#C5283D", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps")

  if (variable == "tmax" || variable == "tmin") {
    suffix = "Temperature (C)"
  } else {
    suffix = "Precipitation (mm)"
  }

  return(list(

    scale_colour_manual(values = myColors),

    theme_minimal(),

    labs(title = plot_title, color = "Dataset",
         x = paste("Gridded", suffix) , y = paste("Mesonet", suffix)),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10))
  ))

}
