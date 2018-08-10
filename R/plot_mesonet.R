organize_data <- function(dat,
                          variable,
                          station_filter,
                          agg_type) {

  if (agg_type == "floor") {
    not_use_value = "mesonet_ceiling"
    use_value = "floor_value"
    use_diff = "floor_diff"
  } else if (agg_type == "ceiling") {
    not_use_value = "mesonet_floor"
    use_value = "ceiling_value"
    use_diff = "ceiling_diff"
  }

  dat %>%
    dplyr::filter(variable == !!variable,
                  station == station_filter,
                  dataset != not_use_value) %>%
    dplyr::mutate(
      dataset = dplyr::if_else(dataset == "mesonet_ceiling" |
                                 dataset == "mesonet_floor",
                               true = "mesonet", false = dataset),
      dataset = factor(dataset),
      station = factor(station)) %>%
    dplyr::rename("diff_value" = use_diff,
                  "mesonet_value" = use_value) %>%
    dplyr::filter(!is.na(value),
                  !is.na(mesonet_value))

}

raw_time_plot <- function(dat_source,
                          variable,
                          station_filter,
                          agg_type) {



  organize_data(dat_source, variable, station_filter, agg_type) %>%
    ggplot2::ggplot(aes(x = date, y = value, color = dataset)) +
      geom_line(size = 0.5) +
      viz_mesonet(variable, "raw_time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~station)
}

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
    geom_sf(data = mcor::mt_state, fill = 'gray40', size = 1, alpha = 0.1) +
    add_hillshade() +
    geom_sf(data = dat, aes(color = Elevation), size = 3) +
    geom_label_repel(data = dat, aes(x = lon, y = lat, label = full_name)) +
    mdt_theme_map() +
    scale_color_distiller(palette = "Oranges",  direction = 1,
                          space = "Lab", name = "Elevation",
                          limit = c(500, 2500)) +
    geom_sf(data = dat, color = "black", pch = 21, size = 3)
}

# #### Function that shows errors in a map ####
# error <- readr::read_csv("./analysis/data/derived_data/Mesonet/error/error_summer_2018.csv",
#                          col_types = readr::cols()) %>%
#   dplyr::filter(station != "lololowr")
#
# error_map <- function(dat, metric, dataset, variable) {
#
#   library(sf)
#   library(ggplot2)
#   library(ggrepel)
#   source("./R/viz_map.R")
#   source("./R/rename_arguments.R")
#
#   dat <- dat %>%
#     dplyr::select(station, dataset, variable, Elevation,
#                   Landform, Aspect, Slope, metric, full_name) %>%
#     dplyr::filter(dataset == !!dataset,
#                   variable == !!variable)
#
#   dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
#     dplyr::right_join(dat) %>%
#     sf::st_transform(102300)
#
#   ggplot2::ggplot() +
#     geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
#     add_hillshade() +
#     geom_sf(data = dat, aes_string(color = metric), size = 3) +
#     mdt_theme_map() +
#     viz_error(metric) +
#     geom_sf(data = dat, color = "black", pch = 21, size = 3) +
#     labs(title = paste(new_metric(metric),
#                        "for",
#                        new_dataset(dataset),
#                        new_variable(variable),
#                        "at Each Mesonet Station"))
#
#   ggplot2::ggsave(filename = paste0("./analysis/figures/mes_analysis/", metric,
#                                     "_", dataset, "_", variable, ".png"),
#                   width = 14, height = 10, units = "in",
#                   device = "png", dpi = "print")
#
# }
#
# mae_time_series <- function(dat, variable) {
#
#   analysis_dates
# }
#
# best_fit_map <- function(dat, metric, variable) {
#
#   library(ggplot2)
#   source("./R/viz_map.R")
#   source("./R/rename_arguments.R")
#
#   best_fit <- function(dataset, value, metric) {
#
#     switch(metric,
#            "r2" = dataset[which(value == max(value))],
#            "mae" = dataset[which(value == min(value))],
#            "mean_bias" = dataset[which(abs(value) == min(abs(value)))],
#            "median_bias" = dataset[which(abs(value) == min(abs(value)))])
#   }
#
#   dat <- dat %>%
#     dplyr::select(station, dataset, variable, Elevation,
#                   Landform, Aspect, Slope, metric) %>%
#     dplyr::filter(variable == !!variable) %>%
#     dplyr::rename(value = metric) %>%
#     dplyr::group_by(station, variable) %>%
#     dplyr::summarise(best = best_fit(dataset, value, metric))
#
#   dat <- sf::read_sf("./analysis/data/raw_data/shapefiles/all_mesonet_attributed.shp") %>%
#     dplyr::right_join(dat) %>%
#     sf::st_transform(102300)
#
#   ggplot2::ggplot() +
#     geom_sf(data = mcor::mt_state, fill = 'gray40', alpha = 0.1) +
#     add_hillshade() +
#     geom_sf(data = dat, aes(color = best), size = 3) +
#     mdt_theme_map() +
#     geom_sf(data = dat, color = "black", pch = 21, size = 3) +
#     viz_best() +
#     labs(title = paste("Most Accurate Gridded Dataset at Each Mesonet Staton\n",
#                        "for",
#                        new_variable(variable),
#                        "Based on",
#                        new_metric(metric)))
#
#   ggplot2::ggsave(filename = paste0("./analysis/figures/mes_analysis/best_",
#                                     metric, "_", variable, ".png"),
#                   width = 14, height = 10, units = "in",
#                   device = "png", dpi = "print")
# }

cumsum_plot <- function(dat_source,
                      variable,
                      station_filter,
                      agg_type) {

  analysis_months <- seq(4, 6)

  organize_data(dat_source, variable, station_filter, agg_type) %>%
    dplyr::group_by(dataset, variable, station) %>%
    dplyr::mutate(cs = cumsum(value)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(aes(x = date, y = cs, color = dataset)) +
      geom_line() +
      facet_wrap(~station) +
      viz_mesonet(variable, "cumsum")


}


direct_plot <- function(dat_source,
                        variable,
                        station_filter,
                        agg_type) {

  organize_data(dat_source, variable, station_filter, agg_type) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot2::ggplot(aes(x = value, y = mesonet_value, color = dataset)) +
      geom_point() +
      geom_abline(intercept = 0, colour = "red", size = 1) +
      viz_mesonet(variable, "direct") +
      coord_fixed() +
      facet_wrap(~station)

}

time_plot <- function(dat_source,
                      variable,
                      station_filter,
                      agg_type) {

  organize_data(dat_source, variable, station_filter, agg_type) %>%
    ggplot(aes(x = date, y = diff_value, color = dataset)) +
      geom_line(size = 0.5) +
      viz_mesonet(variable, "time") +
      geom_hline(yintercept=0, colour="red", size=1) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~station)
}

viz_mesonet <- function(variable, type) {

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD", "#000000")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps","mesonet")

  if (variable == "tmax" || variable == "tmin") {
    suffix <-  "Temperature"
    vunit <- "(C)"
  } else {
    suffix <-  "Precipitation"
    vunit <- "(mm)"
  }

  if (type == "direct") {
    ylab <- paste("Mesonet", suffix, vunit)
    xlab <- paste("Gridded", suffix, vunit)
  } else if (type == "time") {
    ylab <- paste(suffix, "Difference", vunit)
    xlab <- "Date"
  } else if (type == "raw_time") {
    ylab <- paste(suffix, vunit)
    xlab <- "Date"
  } else if (type == "cumsum") {
    ylab <- paste("Cumulative", suffix, vunit)
    xlab <- "Date"
  }

  return(list(

    scale_colour_manual(values = myColors),

    theme_minimal(),

    labs(color = "Dataset",
         x = xlab, y = ylab),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10),
          plot.margin = unit(c(1, 1, 1, 1), "cm"))
  ))

}
