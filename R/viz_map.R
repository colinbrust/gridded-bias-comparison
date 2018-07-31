# These are a set of functions that determine the aesthetics of the maps made
# in the "make_map" function.

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
