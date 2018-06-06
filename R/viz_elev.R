viz_elev <- function(variable, plotTitle, dat) {

  # library(RColorBrewer)
  # library(ggplot2)
  myColors <- c("#FFC857", "#E9724C", "#C5283D", "#481D24", "#255F85", "#F9DBBD")
  names(myColors) <- c("TopoWx", "PRISM", "Ensemble", "Daymet", "Gridmet", "Chirps")

  if (variable == "tmax" || variable == "tmin") {

    return(list(

      scale_colour_manual(name = "Dataset", values = myColors),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = "Elevation" , y = "Median Temperature(C)"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x =  element_text(colour = "gray26", face = "bold"),
            axis.title.y =  element_text(colour = "gray26", face = "bold"),
            legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                         size = 10),
            legend.text =   element_text(colour="gray26", face = "bold", size = 10),
            strip.text =    element_text(family = "sans", size = 9, face = "bold", hjust = 0.5,
                                         vjust = 1))
    ))

  } else {

    return(list(

      scale_colour_manual(name = "Dataset", values = myColors),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = "Elevation", y = "Median Precipitation(mm)"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x =  element_text(colour = "gray26", face = "bold"),
            axis.title.y =  element_text(colour = "gray26", face = "bold"),
            legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                         size = 10),
            legend.text =   element_text(colour="gray26", face = "bold", size = 10),
            strip.text =    element_text(family = "sans", size = 9, face = "bold", hjust = 0.5,
                                         vjust = 1))
    ))

  }

}
