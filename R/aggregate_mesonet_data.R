aggregate_mesonet_data <- function() {

  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(readr)
  source("./R/helpers.R")

  tidy_mesonet <- function(station) {

    list.files("Y:/Data/Mesonet/ZentraTest/API-Output/ClimateOffice/Level1", full.names = T, pattern = station) %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::select(`Record Number [n]`, `Local Date`, `Precipitation [mm]`, `Air Temperature [deg C]`) %>%
      magrittr::set_colnames(c("recordnum", "local_date", "precipitation", "temperature")) %>%
      dplyr::filter(recordnum != "n") %>%
      dplyr::mutate_at(.vars = dplyr::vars(precipitation:temperature),
                       .funs = ~replace_na(.)) %>%
      dplyr::mutate_at(.vars = dplyr::vars(precipitation), # This replaces ppt > 30mm in a 30 min time period.
                       .funs = ~replace_ppt(.)) %>%
      dplyr::mutate(local_date = lubridate::as_datetime(local_date))  %>%
      tibble::add_column(station)

  }

  ceiling_agg <- function(dat) {

    dat %>%
      dplyr::group_by(day = lubridate::ceiling_date(local_date - 43200, "day"), station) %>%
      dplyr::summarise(ppt = sum(precipitation),
                       tmin = min(temperature),
                       tmean = mean(temperature),
                       tmax = max(temperature)) %>%
      tibble::add_column(dataset = "mesonet_ceiling")
  }

  floor_agg <- function(dat) {

    dat %>%
      dplyr::group_by(day = lubridate::floor_date(local_date - 43200, "day"), station) %>%
      dplyr::summarise(ppt = sum(precipitation),
                       tmin = min(temperature),
                       tmean = mean(temperature),
                       tmax = max(temperature)) %>%
      tibble::add_column(dataset = "mesonet_floor")
  }

  mes_list <- list.files("Y:/Data/Mesonet/ZentraTest/API-Output/ClimateOffice/Level1", full.names = T) %>%
    grep("new", ., invert = T, value = T) %>%
    lapply(station_from_fname) %>%
    lapply(tidy_mesonet)

  dplyr::bind_rows(

    mes_list %>%
      lapply(ceiling_agg) %>%
      dplyr::bind_rows() %>% dplyr::ungroup(),

    mes_list %>%
      lapply(floor_agg) %>%
      dplyr::bind_rows() %>% dplyr::ungroup()

  ) %>%
    tidyr::gather(key = "variable", value = "value", -day, -station, -dataset) %>%
    dplyr::mutate(date = lubridate::as_date(day)) %>%
    dplyr::select(-day)
}

