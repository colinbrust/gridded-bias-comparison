error_2017 <- function() {

  library(mcor)

  mes_sites <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp")

  mae_calc <- function(x, y) {

    (x - y) %>%
      abs() %>%
      sum(na.rm = TRUE)
  }

  analysis_dates <- seq(lubridate::as_date("2017-04-01"),
                        lubridate::as_date("2017-07-29"),
                        by = "days")

  dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_2017.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(date %in% analysis_dates)

  stations <- c("conradmt", "corvalli", "ebarllob",
                "havrenmt", "huntleys", "kalispel",
                "moccasin", "sidneymt")

  full_names <- c("Conrad ARC", "Corvallis ARC",
                  "Clearwater SW", "Havre ARC",
                  "Huntley ARC", "Kalispell ARC",
                  "Moccasin ARC", "Sidney ARC")

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
    readr::write_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2017.csv")
}

error <- readr::read_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2017.csv",
                         col_types = readr::cols())

dat1 <- expand.grid(c("r2", "mae", "mean_bias", "median_bias"),
                    c("prism", "gridmet"),
                    c("tmin", "tmax", "ppt"),
                    stringsAsFactors = FALSE)
for(i in 1:nrow(dat1)) {

  error_map(error, dat1$Var1[i], dat1$Var2[i], dat1$Var3[i])
}

dat2 <- expand.grid(c("r2", "mae", "mean_bias", "median_bias"),
                    c("tmin", "tmax", "ppt"),
                    stringsAsFactors = FALSE)

for(i in 1:nrow(dat2)) {

  best_fit_map(error, dat2$Var1[i], dat2$Var2[i])
}
