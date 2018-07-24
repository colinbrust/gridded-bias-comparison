aggregate_mesonet_data <- function(station) {

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

  station_names <- lapply(list.files("./analysis/data/raw_data/mesonet_data", full.names = T), function(x) {
    x %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      stringr::str_split(pattern = "-") %>%
      unlist() %>%
      magrittr::extract(3)
  }) %>% unlist()

  dat <-
    list.files("./analysis/data/raw_data/mesonet_data", full.names = T) %>%
    lapply(readr::read_csv, col_types = readr::cols())

   test <-  mapply(cbind, dat, "station" = station_names, SIMPLIFY = F) %>%
     lapply(tibble::as_tibble) %>%
      dplyr::bind_rows()


  for(i in 2:length(test)) {

    print(colnames(test[[i]]) == colnames(test[[i-1]]))
  }

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
