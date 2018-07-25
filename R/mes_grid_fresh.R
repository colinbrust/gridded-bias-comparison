mes_grid_comp <- function() {

  library(magrittr)
  library(dplyr)
  source("./R/aggregate_mesonet_data.R")
  source("./R/extract_values.R")
  source("./R/helpers.R")

  mes_extract <- function(fname) {

    proj <- mes_sites %>%
              sf::st_transform(fname %>%
                                raster::raster() %>%
                                raster::projection())

    rast <- velox::velox(fname)

    rast$extract_points(sp = proj) %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(dates_from_fname(fname)) %>%
      tibble::add_column(station = mes_sites$station) %>%
      tidyr::gather(key = "date", value = "value", -station) %>%
      tibble::add_column(dataset = dataset_from_fname(fname),
                         variable = variable_from_fname(fname)) %>%
      dplyr::mutate(date = lubridate::as_date(date))
  }

  mes_data <- aggregate_mesonet_data()
  mes_sites <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp")

  list.files("./analysis/data/raw_data/daily_comparison",
             full.names = T,
             pattern = ".tif") %>%
    lapply(mes_extract) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(mes_data)
}

raw_time_plot(test, "tmax", "corvalli") %>% plotly::ggplotly()
