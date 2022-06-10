library(magrittr)

get_mesonet_raw <- function(dirname) {
  
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = rstudioapi::askForPassword("DB Host Name"),
    dbname = rstudioapi::askForPassword("DB Name"),
    user = rstudioapi::askForPassword("DB User"),
    password = rstudioapi::askForPassword("Database password")
  )
  

  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "l1")) %>%
    dplyr::filter(element %in%  c('ppt','air_temp_0200', 'air_temp_0244', 'ppt_max_rate')) %>%
    dplyr::collect() %>% 
    readr::write_csv(file.path(dirname, "raw_mesonet.csv"))
  
}

filter_complete <- function(dat) {
  
  dat %>% 
    dplyr::mutate(
      expected_obs = dplyr::case_when(
        stringr::str_starts(station, 'ace') ~ 288,
        TRUE ~ 96
      )
    ) %>% 
    dplyr::group_by(station, date = lubridate::date(datetime)) %>% 
    dplyr::mutate(
      count = dplyr::n(),
      rate = count / expected_obs  
    ) %>% 
    dplyr::filter(rate >= 0.95) %>% 
    dplyr::select(-c(expected_obs, date, count, rate))
  
}

raw_to_daily <- function(f) {
  
  dat <- readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(element = dplyr::case_when(
      stringr::str_detect(element, '_temp_') ~ 'air_temp',
      TRUE ~ element
    )) %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = element, values_from = value) 
  
  
  dat %>% 
    filter_complete() %>% 
    dplyr::mutate(
      ppt = dplyr::case_when(
        is.na(ppt_max_rate) ~ NaN,
        ppt > 5 ~ NaN,
        TRUE ~ ppt
      )
    ) %>% 
    dplyr::select(-ppt_max_rate) %>% 
    dplyr::group_by(station, date = lubridate::date(datetime)) %>%
    dplyr::summarise(
      tmax = max(air_temp, na.rm = T),
      tmin = min(air_temp, na.rm = T),
      ppt = sum(ppt, na.rm = T)
    ) %>%
    dplyr::arrange(station, date) %>%
    readr::write_csv(file.path(dirname(f), 'daily_mesonet.csv'))
}


