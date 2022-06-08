library(magrittr)


launch_ee <- 
  function(py_path = "/Users/colinbrust/Library/Caches/pypoetry/virtualenvs/py-def-env-L5glzjfy-py3.9/bin/python") {
    reticulate::use_python(py_path)
    rgee::ee_Initialize(drive = TRUE)
    
  }

get_mesonet_shp <- function() {
  readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv", 
                  show_col_types = FALSE) %>%
    dplyr::select(station, name, longitude, latitude, elevation, date_installed) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
}


clean_extraction <- function(dat) {
  dat %>% 
    tidyr::pivot_longer(-station) %>%
    tidyr::separate(name, c("date", "product", "element"), "_") %>% 
    dplyr::mutate(date = lubridate::as_date(date, format = "X%Y%m%d"))
}

get_gridded_data <- function(feature_coll, variable) {
  
  ee <- reticulate::import("ee")
  mesonet <- get_mesonet_shp() %>% 
    dplyr::select(station)
  
  coll <- switch(
    feature_coll, 
    'gridmet' = "IDAHO_EPSCOR/GRIDMET",
    'daymet' = "NASA/ORNL/DAYMET_V4",
    'prism' = "OREGONSTATE/PRISM/AN81d"
  )
  
  resolution <- switch(
    feature_coll, 
    'gridmet' = 4000,
    'daymet' = 1000,
    'prism' = 4000
  )
  
  v <- switch(
    variable,
    'pr' = 'ppt',
    'prcp' = 'ppt',
    'tmmx' = 'tmax',
    'tmmn' = 'tmin', 
    'ppt' = 'ppt',
    'tmax' = 'tmax',
    'tmin' = 'tmin'
  )
  
  print(paste(coll, feature_coll, v, variable))
  
  ee$ImageCollection(coll) %>%
    ee$ImageCollection$filterDate("2016-01-01", "2022-06-01") %>%
    ee$ImageCollection$map(
      function(x) {
        date <- ee$Date(x$get("system:time_start"))$format('YYYY-MM-dd')
        name <- ee$String(glue::glue("{feature_coll}_{v}"))
        x$select(variable)$rename(name)
      }
    ) %>%
      rgee::ee_extract(
      y = mesonet,
      fun = ee$Reducer$mean(),
      scale = resolution,
      sf = F,
      via = "drive",
      container = "GEE_Exports"
    ) %>% 
    clean_extraction()
}

extract_all_data <- function(dirname="./data-raw") {
  
  c("gridmet", "daymet", "prism") %>% 
    purrr::map(function(x) {
      
      vs <- switch(
        x,
        'gridmet' = c("pr", "tmmx", "tmmn"),
        'daymet' = c("prcp", "tmax", "tmin"),
        'prism' = c("ppt", "tmax", "tmin")
      )
      
      purrr::map(vs, function(y) get_gridded_data(x, y)) %>%
        dplyr::bind_rows()
      
    }) %>% 
    dplyr::bind_rows() %>% 
    readr::write_csv(file.path(dirname, 'raw_gridded.csv'))
}

# launch_ee()
# extract_all_data("~/projects/mco/gridded-bias-comparison/data-raw")