# These are a set of functions that determine the aesthetics of the maps made
# in the "make_map" function. They also provide aesthetics for maps made in
# "2018_analysis.R".

# mdt_theme_map, add_hillshade, and get_df are taken from:
# github.com/mt-climate-office/mtdrought/R

# function that removes a lot of the default map aesthetics
# taken from the montana drought github
mdt_theme_map <- function(base_size = 6.5,
                          base_family = "") {
  ggplot2::theme_bw(base_size = base_size,
                    base_family = base_family) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = 'transparent'),
      panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
      legend.background = ggplot2::element_blank(),
      legend.title.align = 0,
      legend.key.width = unit(0.15, "in"),
      legend.key.height = unit(0.15, "in"),
      legend.title = element_text(size = 10, colour = "gray15", face = "bold"),
      legend.text = element_text(family = "sans", colour = "gray26", size = 10,
                                 face = "bold"),
      plot.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "npc"),
      plot.title = element_text(family = "sans", size = 13, hjust = 0.5,
                                colour = "gray15", face = "bold"),
      plot.subtitle = element_text(family = "sans", size = 10, hjust = 0.5,
                                   colour = "gray20", face = "bold"),
      strip.text = element_text(family = "sans", size = 8, face = "bold",
                                hjust = 0.5, vjust = 1))
}

# a function that specifies the color palette for a map.
pal <- function(dev, variable) {
  library(ggplot2)

  if (dev) {
    if (variable == "tmin" || variable == "tmax") {
      return(list(
        scale_fill_gradient2(
          name = "Temperature\nAnomaly (C)",
          high = "#ff0101",
          low = "#014ea8"
        )
      ))

    } else {
      return(list(
        scale_fill_gradient2(
          name = "Precipitation\nAnomaly (mm)",
          high = "#ff0101",
          low = "#014ea8"
        )
      ))

    }

  } else {
    if (variable == "tmin" || variable == "tmax") {
      return(list(
        scale_fill_distiller(
          name = "Temperature (C)",
          palette = "Reds",
          direction = 1
        )
      ))

    } else {
      return(list(
        scale_fill_distiller(
          name = "Precipitation (mm)",
          palette = "Blues",
          direction = 1
        )
      ))

    }
  }
}

# add hillshade to a map of Montana
add_hillshade <- function(){

  # Plot the hillshade using the "alpha hack"
  list(
    ggplot2::geom_raster(data = mcor::mt_hillshade_500m %>%
                           get_df(),
                         mapping = aes(x = x,
                                       y = y,
                                       alpha = ID),
                         na.rm = TRUE),
    scale_alpha(range = c(0.8, 0),
                na.value = 0,
                limits = c(0,255),
                guide = "none")
  )
}

# helper function for add_hillshade
get_df <- function(x){
  library(raster)
  out <- cbind(xyFromCell(x, seq_len(ncell(x))),
               tibble::tibble(ID = getValues(x))) %>%
    tibble::as_tibble()

  if(is.factor(x)){

    levels <- levels(x)[[1]] %>%
      dplyr::mutate_all(.funs = funs(ordered)) %>%
      tibble::as_tibble()

    fact <- out$ID %>%
      ordered(levels = levels(levels$ID))
    out %<>%
      dplyr::mutate(ID = fact) %>%
      dplyr::left_join(levels)
  }

  return(out)
}

# visualization for best_fit_map in "2018_analysis.R"
viz_best <- function() {

  myColors <- c("#FFC857", "#E9724C", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("topowx", "prism", "daymet", "gridmet", "chirps")

  list(scale_color_manual(name = "Dataset", values = myColors))
}

# visualization for error_map in "2018_analysis.R"
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
