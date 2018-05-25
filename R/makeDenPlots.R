# function that creates density based on the parameters put into the function.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)


makeDenPlots <- function(variable, time, stat, ...) {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")
  source("./R/makeTitles.R")

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  plotTitle <- makeTitles(variable, time, stat, c(...))

  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    factorData(time)


  den <-  ggplot2::ggplot(dat, aes(x = Value,  color = Dataset)) +
    geom_density(size = 1) +
    facet_wrap(~Index)

  if(variable == "tmax" || variable == "tmin") {
    den+scale_color_manual(values=tmpPallete)  +
      labs(title = plotTitle[1], subtitle = plotTitle[2], y = "Density", x = "Temperature (°C)") +
      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
  } else {
    den+scale_color_manual(values=pptPallete)  +
      labs(title = plotTitle[1], subtitle = plotTitle[2], y = "Density", x = "Precipitation (mm)") +
      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
  }

}

