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

cumsum_plot <- function(dat_source,
                      variable,
                      station_filter,
                      agg_type) {

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
