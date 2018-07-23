# select_data <- function(variable) {
#
#   "Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/analysis/data/derived_data/Mesonet/extracts" %>%
#     list.files(full.names = TRUE, pattern = variable) %>%
#     readr::read_csv(col_types = readr::cols())
#
# }

raw_time_plot <- function(dat_source,
                          variable,
                          station_filter) {

  dat_source %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::filter(station == station_filter) %>%
    ggplot(aes(x = date, y = value, color = dataset)) +
      geom_line(size = 0.5) +
      viz_mesonet(variable, "raw_time", NA) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~station)

}



direct_plot <- function(dat_source,
                        variable,
                        station_filter) {

  dat_source %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::filter(station == station_filter) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot2::ggplot(aes(x = value, y = mesonet_value, color = dataset)) +
      geom_point() +
      geom_abline(intercept = 0, colour = "red", size = 1) +
      viz_mesonet(variable, "direct", NA) +
      coord_fixed() +
      facet_wrap(~station)

}

time_plot <- function(dat_source,
                      variable,
                      station_filter) {

  dat_source %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::filter(station == station_filter) %>%
    dplyr::filter(dataset != "mesonet") %>%
    ggplot(aes(x = date, y = diff_value, color = dataset)) +
      geom_line(size = 0.5) +
      viz_mesonet(variable, "time", NA) +
      geom_hline(yintercept=0, colour="red", size=1) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~station)
}

var_plot <- function(dat_source,
                     variable,
                     by_var) {

  dat_source %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::group_by(station, dataset, Elevation, Landform, Aspect, Slope) %>%
    dplyr::summarise(avg_diff = mean(diff_value)) %>%
    dplyr::filter(dataset != "mesonet") %>%
    dplyr::ungroup() %>%
    dplyr::select(station, dataset, avg_diff, !!by_var) %>%
    dplyr::rename(by_var = !!by_var) %>%
    ggplot(aes(x = by_var, y = avg_diff, label = station, color = dataset)) +
      geom_point() +
      viz_mesonet(variable, "var", by_var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

viz_mesonet <- function(variable, type, by_var) {

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD", "red")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps", "mesonet")

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
  } else if (type == "var") {
    ylab <- paste("Average", suffix, "Difference")
    xlab <- paste(by_var)
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
          legend.text =   element_text(colour="gray26", face = "bold", size = 10))
  ))

}
