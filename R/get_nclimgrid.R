library(magrittr)

get_mesonet_shp <- function() {
  readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv", 
                  show_col_types = FALSE) %>%
    dplyr::select(station, name, longitude, latitude, elevation, date_installed) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
}

get_nclimgrid <- function(out_dir = "~/data/nclimgrid/raw") {
  tidyr::crossing(
    base_url = "https://noaa-nclimgrid-daily-pds.s3.amazonaws.com/beta/by-month",
    year = 2016:2022,
    month = stringr::str_pad(1:12, 2, pad = "0"),
    element = c("tmax", "tmin", "prcp")
  ) %>%
    dplyr::mutate(
      f_name = glue::glue("{base_url}/{year}/{month}/{element}-{year}{month}-grd-scaled.nc")
    ) %$%
    purrr::map(f_name, function(x) {
      print(x)
      httr::GET(
        x, httr::write_disk(file.path(out_dir, basename(x)), overwrite = T)
      )
    })
}

extract_nclimgrid <- function(data_dir = "~/data/nclimgrid/raw") {
  
  mesonet <- get_mesonet_shp() %>% 
    dplyr::select(station) %>% 
    terra::vect()
  
  list.files(data_dir, full.names = T) %>% 
    purrr::map(function(x) {
      print(x)
      r <- terra::rast(x)
      
      terra::extract(r, mesonet) %>% 
        tibble::as_tibble() %>%
        dplyr::mutate(station = mesonet$station) %>%
        magrittr::set_names(
          c("ID", terra::time(r) %>% as.character(), "station")
        ) %>% 
        dplyr::select(-ID) %>% 
        tidyr::pivot_longer(-station, names_to = 'date') %>%
        dplyr::mutate(
          element = terra::varnames(r),
          product = 'nclimgrid'
        ) %>% 
        dplyr::select(station, date, product, element, gridded_value=value) 
    }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(
      element = dplyr::recode(
        element, 
        "prcp" = "ppt"
      )
    ) %>% 
    readr::write_csv("./data-raw/raw_nclimgrid.csv")
}


