aggregate_mesonet_data <- function() {

  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(readr)
  source("./R/helpers.R")

  replace_na <- function(x){
    x[x>40000] <- NA
    return(x)
  }

  replace_ppt <- function(x) {
    x[x>30] <- NA
    return(x)
  }

  tidy_mesonet <- function(station) {

    list.files("./analysis/data/raw_data/mesonet_data", full.names = T, pattern = station) %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::select(`Record Number [n]`, `UTC Time [ms]`, `Precipitation [mm]`, `Air Temperature [deg C]`) %>%
      magrittr::set_colnames(c("recordnum", "utc_time", "precipitation", "temperature")) %>%
      dplyr::filter(recordnum != "n") %>%
      dplyr::mutate_at(.vars = dplyr::vars(precipitation:temperature),
                       .funs = ~replace_na(.)) %>%
      dplyr::mutate_at(.vars = dplyr::vars(precipitation), # This replaces ppt > 30mm in a 30 min time period.
                       .funs = ~replace_ppt(.)) %>%
      dplyr::mutate(utc_time = lubridate::as_datetime(utc_time))  %>%
      tibble::add_column(station)

  }

  midnight_agg <- function(dat) {

    dat %>%
      dplyr::group_by(day = lubridate::ceiling_date(utc_time, "day"), station) %>%
      dplyr::summarise(ppt = sum(precipitation),
                       tmin = min(temperature),
                       tmean = mean(temperature),
                       tmax = max(temperature)) %>%
      tibble::add_column(dataset = "mesonet_midnight")
  }

  noon_agg <- function(dat) {

    dat %>%
      dplyr::group_by(day = lubridate::ceiling_date(utc_time - 43200, "day"), station) %>%
      dplyr::summarise(ppt = sum(precipitation),
                       tmin = min(temperature),
                       tmean = mean(temperature),
                       tmax = max(temperature)) %>%
      tibble::add_column(dataset = "mesonet_noon")
  }

  mes_list <- lapply(list.files("./analysis/data/raw_data/mesonet_data", full.names = T),
                     station_from_fname) %>%
    lapply(tidy_mesonet)

  dplyr::bind_rows(

    mes_list %>%
      lapply(midnight_agg) %>%
      dplyr::bind_rows() %>% dplyr::ungroup(),

    mes_list %>%
      lapply(noon_agg) %>%
      dplyr::bind_rows() %>% dplyr::ungroup()

  ) %>%
    tidyr::gather(key = "variable", value = "value", -day, -station, -dataset) %>%
    dplyr::mutate(date = lubridate::as_date(day)) %>%
    dplyr::select(-day)
}
