mes_grid_comp <- function(time_period) {

  library(magrittr)
  library(dplyr)
  source("./R/aggregate_mesonet_data.R")
  source("./R/helpers.R")
  source("./R/stack_save_prism_gm.R")

  if(time_period == "current") download_latest()

  mes_extract <- function(fname, time_period, stack_name = NA) {

    if (time_period == "current") {

      date_range <- seq(lubridate::as_date("2017-01-01"),
                        lubridate::as_date(Sys.Date() - 2), by = "day") %>%
        gsub("-", "", .)

      dataset_use <- dataset_from_stack(stack_name)
      variable_use <- variable_from_stack(stack_name)
      print(paste(date_range[1], dataset_use, variable_use))

    } else if (time_period == "2017") {

      date_range <- dates_from_fname(fname)
      dataset_use <- dataset_from_fname(fname)
      variable_use <- variable_from_fname(fname)

    }

    proj <- mes_sites %>%
              sf::st_transform(fname %>%
                                raster::raster() %>%
                                raster::projection())

    rast <- velox::velox(fname)

    rast$extract_points(sp = proj) %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(date_range) %>%
      tibble::add_column(station = mes_sites$station) %>%
      tidyr::gather(key = "date", value = "value", -station) %>%
      tibble::add_column(dataset = dataset_use,
                         variable = variable_use) %>%
      dplyr::mutate(date = lubridate::as_date(date))
  }

  mes_data <- aggregate_mesonet_data()
  mes_sites <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp")


  if (time_period == "current") {

    list_stack <- list_stacks()
    name_vec <- names(list_stack)
    dat <- mapply(mes_extract, fname = list_stack,
           time_period = "current",
           stack_name = name_vec)

    return(
      dplyr::bind_rows(dplyr::bind_cols(dat[,1]),
                      dplyr::bind_cols(dat[,2]),
                      dplyr::bind_cols(dat[,3]),
                      dplyr::bind_cols(dat[,4]),
                      dplyr::bind_cols(dat[,5]),
                      dplyr::bind_cols(dat[,6])) %>%
               dplyr::mutate(value = dplyr::if_else(dataset == "gridmet" &
                                                    (variable == "tmin" |
                                                     variable == "tmax"),
                                                    true = value - 273.15,
                                                    false = value)) %>%
        dplyr::bind_rows(mes_data)
    )
  } else if (time_period == "2017") {

    return(
      list.files("./analysis/data/raw_data/daily_comparison",
                 full.names = T,
                 pattern = ".tif") %>%
        lapply(mes_extract, time_period = "2017") %>%
        dplyr::bind_rows() %>%
        dplyr::bind_rows(mes_data)
    )
  }
}

add_analysis_columns <- function(dat) {

  floor <- dplyr::filter(dat, dataset == "mesonet_floor") %>%
    dplyr::rename(floor_value = value) %>%
    dplyr::select(-dataset)

  ceiling <- dplyr::filter(dat, dataset == "mesonet_ceiling") %>%
    dplyr::rename(ceiling_value = value) %>%
    dplyr::select(-dataset)

  dplyr::left_join(floor, ceiling,
                   by = c("station", "date", "variable")) %>%
    dplyr::right_join(dat,
                      by = c("station", "date", "variable")) %>%
    dplyr::mutate(floor_diff = value - floor_value,
                  ceiling_diff = value - ceiling_value)
}

save_out_table <- function(time_period) {

  if (time_period == "current") {

    mes_grid_comp(time_period) %>%
      add_analysis_columns() %>%
      readr::write_csv("./analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv")

  } else if (time_period == "2017") {

    mes_grid_comp(time_period) %>%
      add_analysis_columns() %>%
      readr::write_csv("./analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv")
  }

}


