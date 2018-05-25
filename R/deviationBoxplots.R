# Function that will show how much each dataset varies from the "ensemble" mean.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"

deviationBoxplots <- function(variable, time, stat, ...) {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")
  source("./R/makeTitles.R")

  asSingular <- function(t) {

    if(t == "Monthly") {return("Month")}
    else if(t == "Seasonal") {return("Season")}
    else if(t == "Annual") {return("30-Year Normal")}
  }

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  plotTitle <- makeTitles(variable, time, stat, c(...), deviation = T)


  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    populate()%>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    factorData(time)

  bp <- ggplot(dat, aes(x = Index, y = EnsDiff, fill = Dataset)) +
    geom_boxplot(color = "gray11")

  if (variable == "tmax" || variable == "tmin") {
    bp+scale_fill_manual(values=tmpPallete)  +
      labs(title = plotTitle[1], subtitle = plotTitle[2], x = asSingular(time), y = "Temperature (°C)") +
      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
  } else {
    bp+scale_fill_manual(values=pptPallete)  +
      labs(title = plotTitle[1], subtitle = plotTitle[2], x = asSingular(time), y = "Precipitation (mm)") +
      theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
            axis.title.x = element_text(colour = "gray26", face = "bold"),
            axis.title.y = element_text(colour = "gray26", face = "bold"),
            legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
            legend.text = element_text(colour="gray26", face = "bold"))
  }
}
