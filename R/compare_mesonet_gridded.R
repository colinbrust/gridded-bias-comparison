# These functions extract gridded data values in pixels corresponding to
# Mesonet station sites. It then saves out the resulting data frame as a csv so
# the data can be plotted.

# function that takes raw mesonet station data and aggregates hourly data into daily data.
daily_mesonet <- function(station) {

  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(readr)

  replace_na <- function(x){
    x[x>40000] <- NA
    return(x)
  }

  replace_ppt <- function(x) {
    x[x>30] <- NA
    return(x)
  }

  dat <-
    list.files("./analysis/data/raw_data/mesonet_data", full.names = T,
             pattern = station) %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::select(`Record Number [n]`, `UTC Time [ms]`, `Local Date`, `Precipitation [mm]`,
                  `Precipitation [mm]`, `Air Temperature [deg C]`) %>%
    magrittr::set_colnames(c("recordnum", "timestamp", "localtime", "precipitation", "temperature")) %>%
    dplyr::filter(recordnum != "n") %>%
    dplyr::mutate_at(.vars = dplyr::vars(precipitation:temperature),
                     .funs = ~replace_na(.)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(precipitation), # This replaces ppt > 30mm in a 30 min time period.
                     .funs = ~replace_ppt(.)) %>%        # This is an arbitrary number but is my filter for now.
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp),
                  noon_time = lubridate::as_datetime(timestamp - 43200)) %>%
    dplyr::distinct()

  dat2 <- dat %>%
    dplyr::group_by(day = lubridate::floor_date(noon_time, "day")) %>%
    dplyr::summarise(ppt = sum(precipitation),
                     tmin = min(temperature),
                     tmean = mean(temperature),
                     tmax = max(temperature)) %>%
    tibble::add_column(dataset = "mesonet_noon")

  dat %>%
    dplyr::group_by(day = lubridate::floor_date(timestamp, "day")) %>%
    dplyr::summarise(ppt = sum(precipitation),
                     tmin = min(temperature),
                     tmean = mean(temperature),
                     tmax = max(temperature)) %>%
    tibble::add_column(dataset = "mesonet") %>%
    dplyr::bind_rows(dat2) %>%
    tibble::add_column(station) %>%
    dplyr::mutate(day = lubridate::as_date(day))
}

# This function returns a tibble of all mesonet station values for a given variable.
extract_mes_vals <- function(variable) {

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_2017.csv",
                  col_types = readr::cols()) %>%
    dplyr::select(day, !!variable, station, dataset) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day,
                  mes_type = dataset)
}

# This function extracts the specified climate variable values for a gridded dataset for
# every day that mesonet station have data.
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

# This function formats a tibble of mesonet data and gridded data so that they
# are in a comparable format.
bind_mes_rows <- function(variable) {

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/mesonet_attributed.shp" %>%
    sf::read_sf()

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_2017.csv",
                  col_types = readr::cols()) %>%
    dplyr::select(day, !!variable, station, dataset) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day) %>%
    dplyr::mutate(mes_type = dataset) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::left_join(mesonet_sites, by = "station") %>%
    dplyr::select(-lat, -lon, -geometry) %>%
    dplyr::mutate(value = mesonet_value)

}

# given a filename, this function returns the date range that the file covers.
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

# given a filename, this function returns the dataset that this file contains
# data for.
dataset_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    head(1)

}

# this function combines all functions above to produce a data frame that
# combines mesonet and gridded data values.
arrange_data <- function(start_date, end_date, variable) {

  library(magrittr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(velox)
  library(ggplot2)

  analysis_dates <- seq(lubridate::as_date(start_date),
                        lubridate::as_date(end_date),
                        by = "days") %>%
    head(-1)

  list.files("./analysis/data/raw_data/daily_comparison",
             full.names = T,
             pattern = ".tif") %>%
    grep(variable, ., value = TRUE) %>%
    lapply(extract_grid_vals, variable = variable) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(bind_mes_rows(variable)) %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::filter(!is.na(mesonet_value)) %>%
    tibble::add_column(variable = !!variable) %>%
    dplyr::mutate(diff_value = value-mesonet_value,
                  station = factor(station),
                  dataset = factor(dataset))
}

# Writes out the most recent mesonet data you downloaded to a tidy format, then
# extracts gridded dataset values for the same locations as mesonet sites and
# also writes out the resulting data frame.
aggregate_mesonet_functions <- function() {

  library(magrittr)

  c("conradmt", "corvalli", "ebarllob", "havrenmt",
   "huntleys", "kalispel", "moccasin", "sidneymt") %>%
    lapply(daily_mesonet) %>%
    dplyr::bind_rows() %>%
    readr::write_csv(path = "./analysis/data/derived_data/Mesonet/all_stations_2017.csv")

  list(arrange_data("2017-01-01", "2018-01-01", "ppt"),
       arrange_data("2017-01-01", "2018-01-01", "tmax"),
       arrange_data("2017-01-01", "2018-01-01", "tmin")) %>%
    dplyr::bind_rows() %>%
    readr::write_csv(path = "./analysis/data/derived_data/Mesonet/extracts/all_20170101_20180101.csv")

}

aggregate_mesonet_functions()
