arrange_data <- function(start_date, end_date, variable) {

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

  list.files("./analysis/data/raw_data/daily_data",
                    full.names = T,
                    pattern = ".tif") %>%
    grep(variable, ., value = TRUE) %>%
    lapply(extract_grid_vals, variable = variable) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(bind_mes_rows(variable)) %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::filter(!is.na(mesonet_value)) %>%
    dplyr::mutate(diff_value = value-mesonet_value,
                  station = factor(station),
                  dataset = factor(dataset))
}

direct_plot <- function(start_date, end_date, variable) {

  plot_title <- paste("Comparison of Gridded and Mesonet", variable, "Values\n",
                      "from", start_date, "to", end_date)

  arrange_data(start_date, end_date, variable) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot2::ggplot(aes(x = value, y = mesonet_value, color = dataset)) +
    geom_point() +
    geom_abline(intercept = 0, colour = "red", size = 1) +
    viz_mesonet(variable, plot_title, "direct") +
    coord_fixed() +
    facet_wrap(~station)

}

time_plot <- function(start_date, end_date, variable) {

  plot_title <- paste("Mesonet Difference from Gridded", variable,  "Values\nfrom",
                      start_date, "to", end_date)

  arrange_data(start_date, end_date, variable) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot(aes(x = date, y = diff_value, color = dataset)) +
      geom_line(size = 0.5) +
      viz_mesonet(variable, plot_title, "time") +
      geom_hline(yintercept=0, colour="red", size=1)+
      facet_wrap(~station)
}

raw_time_plot <- function(start_date, end_date, variable) {

  library(magrittr)

  plot_title <- paste("Mesonet and Gridded", variable,  "Values\nfrom",
                      start_date, "to", end_date)

  arrange_data(start_date, end_date, variable) %>%
    ggplot(aes(x = date, y = value, color = dataset)) +
    geom_line(size = 0.5) +
    viz_mesonet(variable, plot_title, "raw_time") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~station) +
    ylim(0,50)

}

var_plot <- function(start_date, end_date, variable, by_var) {

  dat <- arrange_data(start_date, end_date, variable) %>%
    dplyr::group_by(station, dataset, Elevation, Landform, Aspect, Slope) %>%
    dplyr::summarise(avg_diff = mean(diff_value)) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot(aes(x = station, y = avg_diff, color = dataset)) +
      geom_point() +
      viz_mesonet("variable", "plot_title", "direct") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~station) +
      ylim(0,50)



}

extract_mes_vals <- function(variable) {

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_current.csv",
                  col_types = readr::cols()) %>%
    dplyr::rename(ppt = precipitation) %>%
    dplyr::select(day, !!variable, station) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day) %>%
    dplyr::mutate(date = lubridate::as_date(date))
}

extract_grid_vals <- function(fname, variable) {

  rast <- velox::velox(fname)

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/mesonet_attributed.shp" %>%
    sf::read_sf() %>%
    sf::st_transform(fname %>% raster::raster() %>%
                       raster::projection())

  rast$extract_points(sp = mesonet_sites) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(dates_from_fname(fname)) %>%
    tibble::add_column(station = mesonet_sites$station,
                       Elevation = mesonet_sites$Elevation,
                       Landform = mesonet_sites$Landform,
                       Aspect = mesonet_sites$Aspect,
                       Slope = mesonet_sites$Slope) %>%
    tidyr::gather(key = date, value = value,
                  -station, -Elevation, -Landform, -Slope, -Aspect) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    tibble::add_column(dataset = dataset_from_fname(fname)) %>%
    dplyr::left_join(extract_mes_vals(variable),
                     by = c("station", "date"))


}

bind_mes_rows <- function(variable) {

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/mesonet_attributed.shp" %>%
    sf::read_sf()

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_current.csv",
                  col_types = readr::cols()) %>%
    dplyr::rename(ppt = precipitation) %>%
    dplyr::select(day, !!variable, station) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::left_join(mesonet_sites, by = "station") %>%
    dplyr::select(-lat, -lon, -geometry) %>%
    dplyr::mutate(value = mesonet_value,
                  dataset = "mesonet")

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

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD", "red")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps", "mesonet")

  if (variable == "tmax" || variable == "tmin") {
    suffix <-  "Temperature"
    vunit <- "(C)"
  } else {
    suffix <-  "Precipitation"
    vunit <- "(mm)"
  }

  if (type == "direct") {
    ylab <- paste("Mesonet", suffix, vunit)
    xlab <- paste("Gridded", suffix, vunit)
  } else if (type == "time") {
    ylab <- paste(suffix, "Difference", vunit)
    xlab <- "Date"
  } else if (type == "raw_time") {
    ylab <- paste(suffix, vunit)
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

write_out_csvs <- function(start_date, end_date, variable) {

  fname <- paste0("./analysis/data/derived_data/Mesonet/extracts/",
                  variable, "_",
                  gsub(pattern = "-", "", start_date), "_",
                  gsub(pattern = "_", "", end_date), ".tif")

  arrange_data(start_date, end_date, variable) %>%
    readr::write_csv(path = fname)

}
