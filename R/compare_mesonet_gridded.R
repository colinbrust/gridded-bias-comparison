# These functions extract gridded data values in pixels corresponding to
# Mesonet station sites. It then saves out the resulting data frame as a csv so
# the data can be plotted.
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

write_out_csvs <- function(start_date, end_date, variable) {

  fname <- paste0("./analysis/data/derived_data/Mesonet/extracts/",
                  variable, "_",
                  gsub(pattern = "-", "", start_date), "_",
                  gsub(pattern = "-", "", end_date), ".csv")

  arrange_data(start_date, end_date, variable) %>%
    readr::write_csv(path = fname)

}
