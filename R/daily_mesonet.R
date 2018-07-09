daily_mesonet <- function(station) {

  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(readr)

  replace_na <- function(x){
    x[x>40000] <- NA
    return(x)
  }

  fname = paste0("./analysis/data/derived_data/Mesonet/", station, "_daily.csv")

  valid_times <- seq(as_datetime("2016-01-01 00:00:00"),                # Defines valid times stamps to select
                     as_datetime("2020-12-31 23:30:00"), by = 1800)

  dat <- list.files("./analysis/data/raw_data/mesonet_data", full.names = T,
             pattern = station) %>%
    readr::read_csv() %>%
    dplyr::select(recordnum, timestamp, localdate, precipitation, temperature) %>%
    dplyr::filter(recordnum != "n") %>%
    dplyr::mutate(precipitation = as.numeric(precipitation),
                  temperature = as.numeric(temperature),
                  localdate = paste0(localdate, ":00"),
                  localdate = lubridate::as_datetime(localdate) + 21600) %>% # converts the MDT to UTC
    dplyr::mutate_at(.vars = dplyr::vars(precipitation:temperature),
                     .funs = ~replace_na(.)) %>%
    dplyr::filter(timestamp %in% valid_times) %>%
    dplyr::group_by(days = lubridate::floor_date(localdate, "day")) %>%
    dplyr::summarise(precipitation = sum(precipitation),
                  tmin = min(temperature),
                  tmean = mean(temperature),
                  tmax = max(temperature)) %>%
    readr::write_csv(fname, col_names = T)

}
