#### Arrange data for error mapping ####

error_analysis <- function(year) {

  library(mcor)
  source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/helpers.R")

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

    dat <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv"

  } else if (year == 2017) {

    analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                          lubridate::as_date("2017-12-31"),
                          by = "days")

    dat <- "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv"
  }

  dat %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(date %in% analysis_dates) %>%
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

time_t_test <- function(variable, year, win) {

  dat_in <- mae_analysis(variable, year) %>%
    dplyr::ungroup()

  date_in <- dat_in$date %>%
    unique() %>%
    lapply(function(x) {seq(lubridate::as_date(x),
                            lubridate::as_date(x) + win,
                            by = "days")}) %>%
    head(-win)

  calc_t <- function(analysis_dates, vec) {

    prepped <- dat_in %>%
      dplyr::filter(date %in% analysis_dates) %>%
      dplyr::filter(dataset == vec[1] | dataset == vec[2]) %>%
      dplyr::select(dataset, mae_dataset) %>%
      dplyr::distinct() %>%
      split(.$dataset)

    t.test(prepped[[1]]$mae_dataset, prepped[[2]]$mae_dataset) %>%
      {c(.$p.value, .$estimate[1], .$estimate[2])} %>%
      unname() %>%
      t() %>%
      magrittr::set_colnames(c("pvalue", "mean1", "mean2")) %>%
      tibble::as_tibble() %>%
      tibble::add_column(datasets = paste(names(prepped), collapse = "-"))

  }

  calc_t_app <- function(vec) {

    lapply(date_in, calc_t, vec = vec) %>%
      dplyr::bind_rows()

  }

  dat_out <- dat_in$dataset %>%
    unique() %>%
    utils::combn(2, simplify = FALSE) %>%
    lapply(calc_t_app) %>%
    lapply(function(x) {tibble::add_column(x, "date" = dat_in$date %>%
                                             unique() %>%
                                             head(-win))})

  dat_out %>%
    dplyr::bind_rows() %>%
    tibble::add_column("uniques" = seq(1, length(dat_out)) %>%
                                 lapply(function(x) rep(x, nrow(dat_out[[1]]))) %>%
                                 unlist())

}

plot_t_test <- function(variable, year, win) {

  source("Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/R/helpers.R")

  scaleFUN <- function(x) sprintf("%.2f", x)

  mean_plot <- function(dat) {

    dat %>%
      ggplot2::ggplot() +
      geom_line(aes(x = date, y = mean1, color = 'red'), size = 1) +
      geom_line(aes(x = date, y = mean2, color = 'blue'), size = 1) +
      labs(x = "", y = paste("MAE for Window of", win, "Days"),
           title = paste("T-test Results of", variable, "MAE between\n",
                         datasets_from_column(dat$datasets)[1], "and",
                         datasets_from_column(dat$datasets)[2],
                         "Relative to Montana Mesonet")
           %>% stringr::str_to_title()) +
      viz_mae() +
      scale_color_discrete(name = "Dataset",
                           labels = rev(datasets_from_column(dat$datasets))) +
      scale_y_continuous(labels = scaleFUN)
  }

  p_plot <- function(dat) {

    dat %>%
      ggplot2::ggplot() +
        geom_line(aes(x = date, y = pvalue, color = 'black'), size = 1) +
        ylab("P-value") +
        xlab("Date") +
        ylim(0, 1) +
        geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
        viz_mae() +
        scale_color_manual(name = "P-Value", labels = 'P-Value', values = "black")

  }

  time_t_test(variable, year, win) %>%
    split(.$datasets) %>%
    lapply(function(x) list(mean_plot(x), p_plot(x)))

}

viz_mae <- function() {

  return(list(

    theme_minimal(),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10))
  ))
}
