viz_box <- function(variable, time, plotTitle) {

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  asSingular <- function(t) {

    if(t == "Monthly") {return("Month")}
    else if(t == "Seasonal") {return("Season")}
    else if(t == "Annual") {return("30-Year Normal")}
  }

  if (variable == "tmax" || variable == "tmin") {

    return(list(
      scale_fill_manual(values=tmpPallete),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = asSingular(time), y = "Temperature (C)"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
    ))

  } else {

    return(list(

      bscale_fill_manual(values=pptPallete),

      theme_minimal(),

      labs(title = plotTitle[1], subtitle = plotTitle[2], x = asSingular(time), y = "Precipitation (mm)"),

      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
    ))

  }

}
