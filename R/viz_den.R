viz_den <- function(variable, time, plotTitle) {

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  if (variable == "tmax" || variable == "tmin") {

    return(list(
      scale_color_manual(values=tmpPallete),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = "Temperature (C)" , y = "Density"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
    ))

  } else {

    return(list(

      scale_color_manual(values=pptPallete),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = "Precipitation (mm)", y = "Density"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
    ))

  }

}
