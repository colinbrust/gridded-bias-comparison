#### Arrange data for error mapping ####

error_analysis <- function(year) {

  library(mcor)
  source("./R/helpers.R")

  mes_sites <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp")

  mae_calc <- function(x, y) {

    (x - y) %>%
      abs() %>%
      sum(na.rm = TRUE)
  }

  if(year == 2018) {

    analysis_dates <- seq(lubridate::as_date("2018-04-01"),
                          lubridate::as_date("2018-07-29"),
                          by = "days")

    stations <- stations_2018()[[1]]

    full_names <- stations_2018()[[2]]

    dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv" %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::filter(date %in% analysis_dates)

  } else if (year == 2017) {

    analysis_dates <- seq(lubridate::as_date("2017-04-01"),
                          lubridate::as_date("2017-07-29"),
                          by = "days")

    stations <- stations_2017()[[1]]

    full_names <- stations_2018()[[2]]

    dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv" %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::filter(date %in% analysis_dates)

  }

  temp_error <- dat %>%
    dplyr::filter(station %in% stations) %>%
    dplyr::filter(dataset != "mesonet_ceiling" &
                    dataset != "mesonet_floor",
                  variable != "ppt") %>%
    dplyr::group_by(station, dataset, variable) %>%
    dplyr::summarise(mae_floor   = mae_calc(value, floor_value)/n(),
                     mae_ceiling = mae_calc(value, ceiling_value)/n(),
                     r2_floor    = cor(value, floor_value, method = "pearson",
                                       use = "complete.obs"),
                     r2_ceiling  = cor(value, ceiling_value, method = "pearson",
                                       use = "complete.obs"),
                     mean_bias_fl = mean(floor_diff, na.rm = T),
                     mean_bias_ce = mean(ceiling_diff, na.rm = T),
                     median_bias_fl = median(floor_diff, na.rm = T),
                     median_bias_ce = median(ceiling_diff, na.rm = T))

  # filter to only show months between may and october (inclusive). Also
  # calculate percent MAE instead of raw MAE (Abatzoglou 2013).
  ppt_error <- dat %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::filter(station %in% stations) %>%
    dplyr::filter(dataset != "mesonet_ceiling" &
                    dataset != "mesonet_floor",
                  variable != "tmin" &
                    variable != "tmax") %>%
    dplyr::group_by(station, dataset, variable) %>%
    dplyr::summarise(mae_floor   = mae_calc(value, floor_value)/n(),
                     mae_ceiling = mae_calc(value, ceiling_value)/n(),
                     r2_floor    = cor(value, floor_value, method = "pearson",
                                       use = "complete.obs"),
                     r2_ceiling  = cor(value, ceiling_value, method = "pearson",
                                       use = "complete.obs"),
                     mean_bias_fl = mean(floor_diff, na.rm = T),
                     mean_bias_ce = mean(ceiling_diff, na.rm = T),
                     median_bias_fl = median(floor_diff, na.rm = T),
                     median_bias_ce = median(ceiling_diff, na.rm = T))

  out_name <- paste0("./analysis/data/derived_data/Mesonet/error/error_summer_", year, ".csv")

  dplyr::bind_rows(
    temp_error,
    ppt_error
  ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(station = station,
                     dataset = dataset,
                     variable = variable,
                     mae = dplyr::if_else(dataset == "gridmet",
                                     mae_floor,
                                     mae_ceiling),
                     r2  = dplyr::if_else(dataset == "gridmet",
                                          r2_floor,
                                          r2_ceiling),
                     mean_bias = dplyr::if_else(dataset == "gridmet",
                                                mean_bias_fl,
                                                mean_bias_ce),
                     median_bias = dplyr::if_else(dataset == "gridmet",
                                                  median_bias_fl,
                                                  median_bias_ce)) %>%
    dplyr::right_join(mes_sites,
                      by = "station") %>%
    dplyr::select(-lat, -lon, -geometry) %>%
    dplyr::right_join(tibble(station = stations, full_name = full_names), by = "station") %>%
    readr::write_csv(out_name)
}

mae_analysis <- function(variable, year) {

  library(ggplot2)
  library(magrittr)

  if(year == 2018) {

    analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                          lubridate::as_date("2018-07-29"),
                          by = "days")

    dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv" %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::filter(date %in% analysis_dates)

  } else if (year == 2017) {

    analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                          lubridate::as_date("2017-12-31"),
                          by = "days")

    dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv" %>%
      readr::read_csv(col_types = readr::cols()) %>%
      dplyr::filter(date %in% analysis_dates)
  }

  dat %>%
    dplyr::filter(variable == !!variable,
                  dataset  != "mesonet_ceiling",
                  dataset  != "mesonet_floor") %>%
    dplyr::transmute(station = station,station,
                     date = date,
                     variable = variable,
                     dataset = dataset,
                     value = value,
                     mes_value = dplyr::if_else(dataset == "gridmet",
                                                floor_value,
                                                ceiling_value)) %>%
    dplyr::filter(!is.na(mes_value)) %>%
    dplyr::mutate(abs_error = abs(value - mes_value)) %>%
    dplyr::group_by(date, dataset) %>%
    dplyr::mutate(mae_dataset = sum(abs_error)/dplyr::n())
}


plot_mae_analysis <- function(dat) {

  dat %>%
    ggplot2::ggplot(aes(x = date, y = mae_dataset, color = dataset)) +
    geom_line(size = 1)
}

time_t_test <- function(variable, win) {

  dat_in <- mae_analysis(variable, 2018)

  calc_t <- function(analysis_dates, dat) {

    dat %>%
      dplyr::filter(date %in% analysis_dates) %>%
      dplyr::ungroup() %>%
      dplyr::select(dataset, mae_dataset) %>%
      dplyr::distinct() %>%
      split(.$dataset) %>%
      {t.test(.[[1]]$mae_dataset, .[[2]]$mae_dataset)} %>%
      {c(.$p.value, .$estimate[1], .$estimate[2])} %>%
      unname()
  }

  date_list <- seq(lubridate::as_date("2017-05-01"),
      lubridate::as_date("2018-05-01"),
      by = "days")

  test <- date_list %>%
    lapply(function(x) {seq(lubridate::as_date(x),
                            lubridate::as_date(x) + win,
                            by = "days")}) %>%
    lapply(calc_t, dat = dat_in) %>%
    do.call(rbind, .) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("p_value", "mae_gridmet", "mae_prism")) %>%
    tibble::add_column(date = date_list)

}

plot_t_test <- function(dat, stat) {

  if(stat == "mean") {

    dat %>%
      dplyr::filter(pvalue != 0) %>%
      ggplot2::ggplot() +
        geom_line(aes(x = date, y = mae_prism, color = 'red'), size = 1) +
        geom_line(aes(x = date, y = mae_gridmet, color = 'blue'), size = 1) +
        scale_color_discrete(name = "Dataset", labels = c("Gridmet", "PRISM")) %>%
      return()

  } else if (stat == "p") {

    dat %>%
      dplyr::filter(pvalue != 0) %>%
      ggplot2::ggplot(aes(x = date, y = pvalue)) +
        geom_line(color = 'black', size = 1) +
        scale_color_discrete(name = "p-value", labels = 'p-value') %>%
      return()

  }
}





