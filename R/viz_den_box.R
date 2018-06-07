viz_den_box <- function(variable, time, plotTitle, type) {

  myColors <- c("#FFC857", "#E9724C", "#C5283D", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("TopoWx", "PRISM", "Ensemble", "Daymet", "Gridmet", "Chirps")

  asSingular <- function(t) {

    if(t == "Monthly") {return("Month")}
    else if(t == "Seasonal") {return("Season")}
    else if(t == "Annual") {return("30-Year Normal")}
  }

  if (variable == "tmax" || variable == "tmin")
    legTitle <- "Temperature (C)"
  else
    legTitle <- "Precipitation (mm)"

  return(list(

    if (type == "box")
      scale_fill_manual(name = "Dataset", values = myColors)
    else if (type == "den")
      scale_colour_manual(name = "Dataset", values = myColors),

    theme_minimal(),

    if (type == "box")
      labs(title = plotTitle[1], subtitle = plotTitle[2],
           x = asSingular(time) , y = legTitle)
    else if (type == "den")
      labs(title = plotTitle[1], subtitle = plotTitle[2],
           x = legTitle, y = "Density"),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10))
  ))

}
