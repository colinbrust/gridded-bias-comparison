

calc_error_stats <- function(time_period) {

  mae_calc <- function(x, y) {

    (x - y) %>%
      abs() %>%
      sum(na.rm = TRUE)
  }

  if (time_period == "current") {

    dat <- readr::read_csv("./analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv")

    analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                          lubridate::as_date(Sys.Date() -2),
                          by = "days")

    stations <- c("arskeogh", "bentlake", "blm1arge",
         "blm2virg", "blm3mcca", "blm5kidd",
         "churchil", "conradmt", "corvalli",
         "crowagen", "ebarllob", "ftbentcb",
         "havrenmt", "huntleys", "kalispel",
         "lubrecht", "moccasin", "moltwest",
         "raplejen", "reedpoin", "sidneymt",
         "suatnasa", "turekran")

  } else if (time_period == "2017")  {

    dat <- readr::read_csv("./analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv")

    analysis_dates <- seq(lubridate::as_date("2017-01-01"),
                          lubridate::as_date("2018-01-01"),
                          by = "days") %>%
      head(-1)

    stations <- c("conradmt", "corvalli", "ebarllob",
      "havrenmt", "huntleys", "kalispel",
      "moccasin", "sidneymt")
  }

  temp_error <- dat %>%
    dplyr::filter(date %in% analysis_dates) %>%
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
    dplyr::filter(lubridate::month(date) >= 5 &
                    lubridate::month(date) <= 10,
                  dataset != "mesonet_ceiling" &
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

  dplyr::bind_rows(
    temp_error,
    ppt_error
  ) %>%
    readr::write_csv(paste0("./analysis/data/derived_data/Mesonet/error/error_",
                            time_period, ".csv"))
}
