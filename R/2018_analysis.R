#### Arrange data for error mapping ####

error_2018 <- function() {

  library(mcor)

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

#### Creates reference map showing station names and elevations ####

reference_map <- function() {

  library(sf)
  library(ggrepel)
  source("./R/viz_map.R")

  dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
    sf::st_transform(102300) %>%
    dplyr::mutate(lon= purrr::map_dbl(geometry, ~st_centroid(.x)[[1]]),
                  lat= purrr::map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    dplyr::right_join(readr::read_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2018.csv",
                                      col_types = readr::cols()) %>%
                        dplyr::filter(station != "lololowr")) %>%
    dplyr::filter(dataset == "prism", variable == "tmax")

    ggplot2::ggplot() +
      geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
      add_hillshade() +
      geom_sf(data = dat, aes(color = Elevation), size = 3) +
      geom_label_repel(data = dat, aes(x = lon, y = lat, label = full_name)) +
      mdt_theme_map() +
      scale_color_distiller(palette = "Spectral",  direction = -1,
                            space = "Lab", name = "Elevation",
                            limit = c(500, 2500)) +
      geom_sf(data = dat, color = "black", pch = 21, size = 3) +
      labs(title = "Location and Elevation of Montana Mesonet Sites")
}

#### Function that shows errors in a map ####
error <- readr::read_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2018.csv",
                                  col_types = readr::cols()) %>%
  dplyr::filter(station != "lololowr")

error_map <- function(dat, metric, dataset, variable) {

  library(sf)
  library(ggplot2)
  library(ggrepel)
  source("./R/viz_map.R")
  source("./R/rename_arguments.R")

  dat <- dat %>%
    dplyr::select(station, dataset, variable, Elevation,
                  Landform, Aspect, Slope, metric, full_name) %>%
    dplyr::filter(dataset == !!dataset,
                  variable == !!variable)

  dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
    dplyr::right_join(dat) %>%
    sf::st_transform(102300)

  ggplot2::ggplot() +
    geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
    add_hillshade() +
    geom_sf(data = dat, aes_string(color = metric), size = 3) +
    mdt_theme_map() +
    viz_error(metric) +
    geom_sf(data = dat, color = "black", pch = 21, size = 3) +
    labs(title = paste(new_metric(metric),
                       "for",
                       new_dataset(dataset),
                       new_variable(variable),
                       "at Each Mesonet Station"))

  ggplot2::ggsave(filename = paste0("./analysis/figures/mes_analysis/", metric,
                                    "_", dataset, "_", variable, ".png"),
                  width = 14, height = 10, units = "in",
                  device = "png", dpi = "print")

}

#### Map the best dataset for each station location ####

best_fit_map <- function(dat, metric, variable) {

  library(ggplot2)
  source("./R/viz_map.R")
  source("./R/rename_arguments.R")

  best_fit <- function(dataset, value, metric) {

    switch(metric,
           "r2" = dataset[which(value == max(value))],
           "mae" = dataset[which(value == min(value))],
           "mean_bias" = dataset[which(abs(value) == min(abs(value)))],
           "median_bias" = dataset[which(abs(value) == min(abs(value)))])
  }

 dat <- dat %>%
    dplyr::select(station, dataset, variable, Elevation,
                  Landform, Aspect, Slope, metric) %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::rename(value = metric) %>%
    dplyr::group_by(station, variable) %>%
    dplyr::summarise(best = best_fit(dataset, value, metric))

  dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
    dplyr::right_join(dat) %>%
    sf::st_transform(102300)

  ggplot2::ggplot() +
    geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
   add_hillshade() +
    geom_sf(data = dat, aes(color = best), size = 3) +
    mdt_theme_map() +
    geom_sf(data = dat, color = "black", pch = 21, size = 3) +
    viz_best() +
    labs(title = paste("Most Accurate Gridded Dataset at Each Mesonet Staton\n",
                       "for",
                       new_variable(variable),
                       "Based on",
                       new_metric(metric)))

  ggplot2::ggsave(filename = paste0("./analysis/figures/mes_analysis/best_",
                                    metric, "_", variable, ".png"),
                  width = 14, height = 10, units = "in",
                  device = "png", dpi = "print")
}

dat1 <- expand.grid(c("r2", "mae", "mean_bias", "median_bias"),
                    c("prism", "gridmet", "daymet"),
                    c("tmin", "tmax", "ppt"),
                    stringsAsFactors = FALSE)
for(i in 1:nrow(dat1)) {

  error_map(dat1$Var1[i], dat1$Var2[i], dat1$Var3[i])
}

dat2 <- expand.grid(c("r2", "mae", "mean_bias", "median_bias"),
                    c("tmin", "tmax", "ppt"),
                    stringsAsFactors = FALSE)

for(i in 1:nrow(dat2)) {

  best_fit_map(dat2$Var1[i], dat2$Var2[i])
}
