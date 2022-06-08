library(magrittr)


get_mesonet_raw <- function(dirname) {
  
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = rstudioapi::askForPassword("DB Host Name"),
    dbname = rstudioapi::askForPassword("DB Name"),
    user = rstudioapi::askForPassword("DB User"),
    password = rstudioapi::askForPassword("Database password")
  )
  

  dplyr::tbl(con, RPostgres::Id(schema = "data", table = "observations")) %>%
    dplyr::filter(element == 'ppt' |
                    element == 'air_temp_0200' | element == 'air_temp_0244') %>%
    dplyr::collect() %>%
    readr::write_csv(file.path(dirname, "raw_mesonet.csv"))
  
}

raw_to_daily <- function(f) {
  readr::read_csv(f) %>%
    dplyr::mutate(element = dplyr::case_when(
      stringr::str_detect(element, '_temp_') ~ 'air_temp',
      TRUE ~ element
    )) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider(names_from = element, values_from = value) %>%
    dplyr::group_by(station, date = lubridate::date(datetime)) %>%
    dplyr::summarise(
      tmax = max(air_temp, na.rm = T),
      tmin = min(air_temp, na.rm = T),
      tmean = mean(air_temp, na.rm = T),
      ppt = sum(ppt, na.rm = T)
    ) %>%
    dplyr::arrange(station, date) %>%
    readr::write_csv(file.path(dirname(f), 'daily_mesonet.csv'))
}