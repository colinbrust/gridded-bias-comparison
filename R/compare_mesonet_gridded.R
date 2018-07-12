arrange_data <- function(start_date, end_date, variable) {

  library(magrittr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(velox)
  library(ggplot2)
  library(ggpmisc)

  # start_date = "2017-09-01"
  # end_date = "2017-10-01"
  # variable = "tmin"

  analysis_dates <- seq(lubridate::as_date(start_date),
                     lubridate::as_date(end_date),
                     by = "days") %>%
    head(-1)

  list.files("./analysis/data/raw_data/daily_data",
                    full.names = T,
                    pattern = ".tif") %>%
    grep(variable, ., value = TRUE) %>%
    lapply(extract_grid_vals) %>%
    dplyr::filter(dates %in% analysis_date) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(mesonet_value)) %>%
    dplyr::mutate(diff_value = grid_value-mesonet_value,
                  station = factor(station),
                  dataset = factor(dataset))
}

direct_plot <- function(start_date, end_date, variable) {

  plot_title <- paste("Comparison of Gridded and Mesonet", variable, "Values\n",
                      "from", start_date, "to", end_date)

  arrange_data(start_date, end_date, variable) %>%
    ggplot2::ggplot(aes(x = grid_value, y = mesonet_value, color = dataset)) +
    geom_point() +
    viz_mesonet(variable, plot_title, "direct") +
    facet_wrap(~station)

}

time_plot <- function(start_date, end_date, variable) {

  plot_title <- paste("Mesonet Difference from Gridded", variable,  "Values\nfrom",
                      start_date, "to", end_date)

  arrange_data(start_date, end_date, variable) %>%
    ggplot(aes(x = date, y = diff_value, color = dataset)) +
      geom_line(size = 1) +
      viz_mesonet(variable, plot_title, "time") +
      facet_wrap(~station)
}

var_plot <- function(start_date, end_date, variable, by_var) {

  dat <- arrange_data(start_date, end_date, variable) %>%
    dplyr::group_by(station, dataset) %>%
    dplyr::mutate(avg_diff = mean(diff_value))

}

extract_mes_vals <- function(variable) {

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations.csv",
                  col_types = readr::cols()) %>%
    dplyr::rename(ppt = precipitation) %>%
    dplyr::select(days, !!variable, station_name) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = days,
                  station = station_name) %>%
    dplyr::mutate(date = lubridate::as_date(date))
}

extract_grid_vals <- function(fname) {

  rast <- velox::velox(fname)

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/mesonet_attributed.shp" %>%
    sf::read_sf() %>%
    sf::st_transform(fname %>% raster::raster() %>%
                       raster::projection())

  rast$extract_points(sp = mesonet_sites) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(dates_from_fname(fname)) %>%
    tibble::add_column(station = mesonet_sites$station,
                       elevation = mesonet_sites$Elevation,
                       landform = mesonet_sites$Landform,
                       aspect = mesonet_sites$Aspect,
                       slope = mesonet_sites$Slope) %>%
    tidyr::gather(key = date, value = grid_value,
                  -station, -elevation, -landform) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    tibble::add_column(dataset = dataset_from_fname(fname)) %>%
    dplyr::left_join(extract_mes_vals(variable),
                     by = c("station", "date"))

}

dates_from_fname <- function(fname) {

  dates <-
    fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    tail(2) %>%
    lubridate::as_date()

  seq(dates[1], dates[2], by = "days") %>%
    head(-1)

}

dataset_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    head(1)

}





viz_mesonet <- function(variable, plot_title, type) {

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps")

  if (variable == "tmax" || variable == "tmin") {
    suffix <-  "Temperature (C)"
  } else {
    suffix <-  "Precipitation (mm)"
  }

  if (type == "direct") {
    ylab <- paste("Mesonet", suffix)
    xlab <- paste("Gridded", suffix)
  } else if (type == "time") {
    ylab <- "Temperature Difference (C)"
    xlab <- "Date"
  }

  return(list(

    scale_colour_manual(values = myColors),

    theme_minimal(),

    labs(title = plot_title, color = "Dataset",
         x = xlab, y = ylab),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10))
  ))

}

