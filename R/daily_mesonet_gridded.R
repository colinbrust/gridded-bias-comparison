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

  list.files("Y:/Data/Mesonet/ZentraTest/API-Output/ClimateOffice/Level1", full.names = T,
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
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(day = lubridate::floor_date(timestamp, "day")) %>%
    dplyr::summarise(ppt = sum(precipitation),
                     tmin = min(temperature),
                     tmean = mean(temperature),
                     tmax = max(temperature)) %>%
    tibble::add_column(station)

}

# This function returns a tibble of all mesonet station values for a given variable.
extract_mes_vals <- function(variable) {

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_current.csv",
                  col_types = readr::cols()) %>%
    dplyr::select(day, !!variable, station) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day) %>%
    dplyr::mutate(date = lubridate::as_date(date))
}

# This function extracts the specified climate variable values for a gridded dataset for
# every day that mesonet station have data.
extract_grid_vals <- function(rast_stack, variable, dataset) {

  rast <- velox::velox(rast_stack)

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp" %>%
    sf::read_sf() %>%
    sf::st_transform(rast_stack %>%
                       raster::projection())

  date_range <- seq(lubridate::as_date("2017-01-01"),
                    lubridate::as_date(last_image_date(dataset)), by = "days") %>%
    gsub("-", "", .)

  rast$extract_points(sp = mesonet_sites) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(date_range) %>%
    tibble::add_column(station = mesonet_sites$station,
                       Elevation = mesonet_sites$Elevation,
                       Landform = mesonet_sites$Landform,
                       Aspect = mesonet_sites$Aspect,
                       Slope = mesonet_sites$Slope) %>%
    tidyr::gather(key = date, value = value,
                  -station, -Elevation, -Landform, -Slope, -Aspect) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    tibble::add_column(dataset = !!dataset) %>%
    dplyr::left_join(extract_mes_vals(variable),
                     by = c("station", "date"))


}

# This function formats a tibble of mesonet data and gridded data so that they
# are in a comparable format.
bind_mes_rows <- function(variable) {

  mesonet_sites <- "./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp" %>%
    sf::read_sf()

  readr::read_csv("./analysis/data/derived_data/Mesonet/all_stations_current.csv",
                  col_types = readr::cols()) %>%
    dplyr::select(day, !!variable, station) %>%
    dplyr::rename(mesonet_value = !!variable,
                  date = day) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::left_join(mesonet_sites, by = "station") %>%
    dplyr::select(-lat, -lon, -geometry) %>%
    dplyr::mutate(value = mesonet_value,
                  dataset = "mesonet")

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
arrange_data <- function(variable) {

  dat <-  mapply(extract_grid_vals,
         rast_stack = list(stack_gridmet(variable), stack_prism(variable)),
         variable = list(variable, variable),
         dataset = list("gridmet", "prism"))

  print(paste(variable, "is extracted"))

  dplyr::bind_rows(dplyr::bind_cols(dat[,1]),
                   dplyr::bind_cols(dat[,2])) %>%
    dplyr::bind_rows(bind_mes_rows(variable)) %>%
    dplyr::filter(!is.na(mesonet_value)) %>%
    tibble::add_column(variable = !!variable) %>%
    dplyr::mutate(value = dplyr::if_else(dataset == "gridmet" &
                                        (variable == "tmin" |
                                         variable == "tmax"),
                  true = value - 273.15,
                  false = value)) %>%
    dplyr::mutate(diff_value = value-mesonet_value,
                  station = factor(station),
                  dataset = factor(dataset))
}

# Writes out the most recent mesonet data you downloaded to a tidy format, then
# extracts gridded dataset values for the same locations as mesonet sites and
# also writes out the resulting data frame.
aggregate_mesonet_functions <- function() {

  library(magrittr)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(velox)
  library(ggplot2)
  source("./R/stack_save_prism_gm.R")

  list.files("./analysis/data/derived_data/Mesonet/extracts", full.names = T,
             pattern = "mes_grid") %>%
    unlink()

  download_latest()

  suppressWarnings(c("arskeogh", "bentlake", "blm1arge", "blm2virg", "blm3mcca",
    "blm5kidd", "churchil", "conradmt", "corvalli", "crowagen",
    "ebarllob", "ftbentcb", "havrenmt", "huntleys", "kalispel",
    "lubrecht", "moccasin", "moltwest", "raplejen", "reedpoin",
    "sidneymt", "suatnasa", "turekran") %>%
    lapply(daily_mesonet) %>%
    dplyr::bind_rows() %>%
    readr::write_csv(path = "./analysis/data/derived_data/Mesonet/all_stations_current.csv"))

  out_name <- paste0("./analysis/data/derived_data/Mesonet/extracts/mes_grid_20170101_",
                     gsub("-", "", as.character(Sys.Date() - 1)), ".csv")

  list(arrange_data("ppt"),
       arrange_data("tmax"),
       arrange_data("tmin")) %>%
    dplyr::bind_rows() %>%
    readr::write_csv(path = out_name)

}

aggregate_mesonet_functions()


