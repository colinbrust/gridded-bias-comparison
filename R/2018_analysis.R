library(mcor)



error_2018 <- function() {

  mes_sites <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp")

  mae_calc <- function(x, y) {

    (x - y) %>%
      abs() %>%
      sum(na.rm = TRUE)
  }

  analysis_dates <- seq(lubridate::as_date("2018-04-01"),
                        lubridate::as_date("2018-07-29"),
                        by = "days")

  dat <- "./analysis/data/derived_data/Mesonet/extracts/mes_grid_current.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(date %in% analysis_dates)

  stations <- c("arskeogh", "bentlake", "blm1arge",
                "blm2virg", "blm3mcca", "blm5kidd",
                "churchil", "conradmt", "corvalli",
                "crowagen", "ebarllob", "ftbentcb",
                "havrenmt", "huntleys", "kalispel",
                "lubrecht", "moccasin", "moltwest",
                "raplejen", "reedpoin", "sidneymt",
                "suatnasa", "turekran")

  full_names <- c("Fort Keogh ARS N", "Benton Lake W",
                  "Argenta BLM", "Virginia City BLM",
                  "McCartney Mtn BLM", "Kidd BLM",
                  "Churchill", "Conrad ARC",
                  "Corvallis ARC", "Crow Agency E",
                  "Clearwater SW", "Fort Benton E",
                  "Havre ARC", "Huntley ARC",
                  "Kalispell ARC", "Lubrecht Forest",
                  "Moccasin ARC", "Molt W",
                  "Rapleje N", "Reed Point NE",
                  "Sidney ARC", "Suat",
                  "Turek Ranch")


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
    readr::write_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2018.csv")
}




error <- readr::read_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2018.csv") %>%
  dplyr::filter(station != "lololowr")

error_map <- function(dat, metric, dataset, variable) {

  library(sf)
  library(ggrepel)
  source("./R/viz_map.R")

  dat <- dat %>%
    dplyr::select(station, dataset, variable, Elevation,
                  Landform, Aspect, Slope, metric, full_name) %>%
    dplyr::filter(dataset == !!dataset,
                  variable == !!variable)

  dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
    dplyr::right_join(dat) %>%
    sf::st_transform(102300) %>%
    mutate(lon= purrr::map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat= purrr::map_dbl(geometry, ~st_centroid(.x)[[2]]))

  ggplot2::ggplot() +
    geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
    geom_sf(data = dat, aes_string(color = metric), size = 3) +
    geom_label_repel(data = dat, aes(x = lon, y = lat, label = full_name)) +
    mdt_theme_map() +
    viz_error(metric) +
    geom_sf(data = dat, color = "black", pch = 21, size = 3)

}

viz_error <- function(metric) {

  if(metric == "mae") {

    return(list(
      scale_color_distiller(palette = "Reds",  direction = 1,
                            space = "Lab", name = "Mean Absolute\nError",
                            limit = c(0, 4.1))
    ))
  } else if (metric == "r2") {

    return(list(
      scale_color_distiller(palette = "Reds",  direction = 1,
                            space = "Lab", name = "Pearson's r\nCorrelation",
                            limit = c(0, 1))
    ))
  } else if (metric == "mean_bias") {
    return(list(
      scale_color_distiller(palette = "RdBu",  direction = -1,
                            space = "Lab", name = "Mean Bias",
                            limit = c(-3, 4))
    ))
  } else if (metric == "median_bias") {
    return(list(
      scale_color_distiller(palette = "RdBu",  direction = -1,
                            space = "Lab", name = "Median Bias",
                            limit = c(-3, 4))
    ))
  }

}




best_fit_map <- function(dat, metric, variable) {

  dat %>%
    dplyr::select(station, dataset, variable, Elevation,
                  Landform, Aspect, Slope, metric) %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::rename(value = metric) %>%
    dplyr::group_by(station, variable) %>%
    {dplyr::summarise(., best = best_fit(., metric))}
}

best_fit <- function(dat, metric) {

  if(metric == "mae" | metric == "mean_bias" | metric == "median_bias")
    dat$dataset[dat$value == min(dat$value)]
  else
    dat$dataset[dat$value == max(dat$value)]

}

