library(magrittr)

arrange_ppt_data <- function() {

"Y:/Projects/MCO_Gridded_Met_Eval/GriddedPackage/mesonet_app/data/new_error_analysis.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(date <= lubridate::as_date("2018-07-25"), # for some reason mesonet data is missing on the 26th of July
                  variable == "ppt",
                  !is.na(floor_value),
                  lubridate::month(date) >= 4 & lubridate::month(date) <= 10) %>%
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
    dplyr::select(-floor_value, -ceiling_value, -floor_diff, -ceiling_diff) %>%
    dplyr::group_by(date, dataset) %>%
    dplyr::mutate(percent_correct = sum(grd_correct)/dplyr::n()) %>%
    dplyr::ungroup()
}

binary_plot <- function() {

  library(ggplot2)

  arrange_ppt_data() %>%
    dplyr::filter(!is.na(mes_value)) %>%
    dplyr::select(date, dataset, percent_correct) %>%
    dplyr::distinct() %>%
    ggplot(aes(x = date, y = percent_correct, color = dataset)) +
      geom_point(size = 1)

}


# mes_tf = T or F
ppt_den_plot <- function(mes_tf, plot_type) {

  library(ggplot2)

  if (plot_type == "ecdf") use_plot <- match.fun(ggplot2::stat_ecdf)
  else if (plot_type == "den") use_plot <- match.fun(ggplot2::geom_density)

  if(mes_tf) word <- "Recorded Precipitation"
  else word <- "Didn't Record Precipitation"

  p_title <- paste("Distribution of Bias on Days Mesonet", word)

  arrange_ppt_data() %>%
    dplyr::filter(!is.na(mes_value),
                  mes_ppt == mes_tf) %>%
    dplyr::mutate(ppt_bin = dplyr::ntile(bias, 100)) %>%
    # dplyr::group_by(dataset, ppt_bin) %>%
    # dplyr::summarise(median = median(bias, na.rm = T),
    #                  Q1 = as.numeric(quantile(bias)[2]),
    #                  Q3 = as.numeric(quantile(bias)[4])) %>%
    ggplot(aes(x = bias, color = dataset)) +
      use_plot(size = 1) +
      viz_ppt() +
      labs(x = "Median Bias", y = "Density",
           color = "Dataset", title = p_title)

}

bias_time_plot <- function(dataset1, dataset2) {

  arrange_ppt_data() %>%
    dplyr::filter(dataset == dataset1 | dataset == dataset2) %>%
    dplyr::filter(!is.na(mes_value)) %>%
    dplyr::group_by(date, dataset) %>%
    dplyr::
    ggplot(aes(x = date, y = bias, color = dataset)) +
      geom_line(size = 1)
}


viz_ppt <- function() {

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD", "#000000")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps","mesonet")

  return(list(
    scale_colour_manual(values = myColors),

    theme_minimal(),

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
