library(magrittr)

arrange_ppt_data <- function() {

  "./data/new_error_analysis.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(date <= lubridate::as_date("2018-07-25"), # for some reason mesonet data is missing on the 26th of July
                  variable == "ppt",
                  !is.na(floor_value)) %>%
    dplyr::mutate(mes_value = dplyr::if_else(dataset == "gridmet",
                                             floor_value,
                                             ceiling_value),
                  bias = dplyr::if_else(dataset == "gridmet",
                                        floor_diff,
                                        ceiling_diff),
                  mes_ppt = dplyr::if_else(mes_value == 0,
                                           0, 1),
                  grd_ppt = dplyr::if_else(value == 0,
                                           0, 1),
                  grd_correct = dplyr::if_else(mes_ppt == grd_ppt,
                                               1, 0)) %>%
    dplyr::select(-floor_value, -ceiling_value, -floor_diff, -ceiling_diff)
}

binary_plot <- function(station) {

  library(ggplot2)

  arrange_ppt_data() %>%
    dplyr::filter(!is.na(mes_value),
                  station == !!station) %>%
    ggplot(aes(x = date, y = grd_correct, color = dataset)) +
      geom_jitter(size = 2, alpha = 0.4,
                  position = position_jitter(height = .02)) +
      stat_smooth(method = 'loess',size = 1)

}

ppt_den_plot <- function()
