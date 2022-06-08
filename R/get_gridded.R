library(magrittr)


launch_ee <- 
  function(py_path = "~/.cache/pypoetry/virtualenvs/py-def-env-t7YCAhar-py3.8/bin/python") {
    reticulate::use_python(py_path)
    rgee::ee_Initialize()
    
  }


get_mesonet_shp <- function() {
  readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv") %>%
    dplyr::select(station, name, longitude, latitude, elevation, date_installed) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
}

get_gridded_data <- function(feature_coll, variable) {
  
  ee <- reticulate::import("ee")
  mesonet <- get_mesonet_shp()
  
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
    'tmmn' = 'tmin'
  )
  
  ee$ImageCollection(coll) %>%
    ee$ImageCollection$filterDate("2016-01-01", "2022-05-01") %>%
    ee$ImageCollection$map(
      function(x) {
        date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
        name <- ee$String$cat(glue::glue("{feature_coll}_{v}_"), date)
        x$select(variable)$rename(name)
      }
    ) %>%
      rgee::ee_extract(
      y = mesonet,
      fun = ee$Reducer$mean(),
      scale = resolution,
      sf = T,
      via = "drive",
      container = "GEE_Exports"
    )
}


