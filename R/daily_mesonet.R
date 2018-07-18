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

  valid_times <- seq(as_datetime("2016-01-01 00:00:00"),
                     as_datetime("2020-12-31 23:30:00"), by = 1800)

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
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(day = lubridate::floor_date(timestamp, "day")) %>%
    dplyr::summarise(precipitation = sum(precipitation),
                  tmin = min(temperature),
                  tmean = mean(temperature),
                  tmax = max(temperature)) %>%
    tibble::add_column(station)

}

c("conradmt", "corvalli", "ebarllob", "havrenmt",
           "huntleys", "kalispel", "moccasin", "sidneymt") %>%
  lapply(daily_mesonet) %>%
  dplyr::bind_rows() %>%
  readr::write_csv(path = "./analysis/data/derived_data/Mesonet/all_stations_current.csv")




