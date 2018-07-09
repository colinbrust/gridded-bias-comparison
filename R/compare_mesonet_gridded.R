compare_mesonet_gridded <- function(start_date, end_date, variable) {

  library(magrittr)
  library(lubridate)
  library(dplyr)
  library(tidyr)


  analysis_dates <- seq(lubridate::as_date(start_date),
                     lubridate::as_date(end_date),
                     by = "days") %>%
    head(-1)

  dat <- list.dirs("./analysis/data/raw_data/daily_data",
                             full.names = T) %>%
    tail(-1) %>%
    list.files(recursive = T, full.names = T) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames("filename") %>%
    dplyr::mutate(base = basename(filename) %>%
                    tools::file_path_sans_ext()) %>%
    tidyr::separate(base,
                    into = c("dataset", "var", "date"),
                    by = "_") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'pr' | var == 'prcp',
                                      'ppt',
                                      var)) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'tmmn',
                                      'tmin',
                                      var)) %>%
    dplyr::mutate(var = dplyr::if_else(var == 'tmmx',
                                      'tmax',
                                      var)) %>%
    dplyr::filter(var == variable)

}

extract_mes_vals <- function(fname) {

  library(velox)

  rast <- velox::velox(fname)

  mesonet_sites <- readr::read_csv(
    "./analysis/data/raw_data/spreadsheets/station_locations.csv"
  ) %>%
    sf::st_as_sf(coords = c("lat", "lon"),
                 crs = 4326, agr = 'constant') %>%
    sf::st_transform(raster::projection(
      raster::raster(fname)
    ))



}
