library(ggplot2)
library(readr)
library(tibble)
library(tidyr)
library(wesanderson)


tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")


dataDir <- "/Users/cbandjelly/Desktop/MCO/Gridded_Data/Outputs/"


### variable = "tmax", "tmin", or "ppt"
#time = "Month", "Season", or "Year"
#statistic = "Normal" or "SD"
#type = "box", "ecdf", or "den"
makePlot <- function(variable, time, statistic, type) {

  if(time == "Year") {plotDat <- makeLongMatrix(variable, "Annual", statistic)
  } else {
    plotDat <- makeLongMatrix(variable, time, statistic)
  }

  print("Long matrix created... Making plot")
  if (type == "box") {

    plotDat$Dataset <- factor(plotDat$Dataset)

    if(time == "Month") {
      plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, plotDat$Statistic[1], "by", time))
      plotDat$Time <- factor(plotDat$Time, levels = month.name)
    } else if (time == "Season") {
      plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, plotDat$Statistic[1], "by", time))
      plotDat$Time <- factor(plotDat$Time, levels = c("Winter", "Spring", "Summer", "Autumn"))
    } else if (time == "Year") {
      plotTitle <- tools::toTitleCase(paste("Boxplot of 30-Year", variable, plotDat$Statistic[1]))
    }

    bp <- ggplot2::ggplot(plotDat, aes(x = Time, y = Value, fill = Dataset)) +
      geom_boxplot(color = "gray11")

    if(variable == "tmax" || variable == "tmin") {
      bp+scale_fill_manual(values=tmpPallete)  +
        labs(x = time, y = "Temperature (°C)") +
        ggtitle(plotTitle) +
        theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
              axis.title.x = element_text(colour = "gray26", face = "bold"),
              axis.title.y = element_text(colour = "gray26", face = "bold"),
              legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
              legend.text = element_text(colour="gray26", face = "bold"))
    } else {
      bp+scale_fill_manual(values=pptPallete)  +
        labs(x = time, y = "Precipitation (mm)") +
        ggtitle(plotTitle) +
        theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
              axis.title.x = element_text(colour = "gray26", face = "bold"),
              axis.title.y = element_text(colour = "gray26", face = "bold"),
              legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
              legend.text = element_text(colour="gray26", face = "bold"))
    }


  } else if (type == "den") {

    if(time == "Month") {
      plotTitle <- tools::toTitleCase(paste("Density Plot of", variable, plotDat$Statistic[1], "by", time))
    } else if (time == "Season") {
      plotTitle <- tools::toTitleCase(paste("Density Plot of", variable, plotDat$Statistic[1], "by", time))
    } else if (time == "Year") {
      plotTitle <- tools::toTitleCase(paste("Density Plot of 30-Year", variable, plotDat$Statistic[1]))
    }

    den <-  ggplot2::ggplot(plotDat, aes(x = Value,  color = Dataset)) +
      geom_density() +
      facet_wrap(~Time)

    if(variable == "tmax" || variable == "tmin") {
      den+scale_color_manual(values=tmpPallete)  +
        labs(y = "Percent", x = "Temperature (°C)") +
        ggtitle(tools::toTitleCase(paste("Density Plot of", variable, plotDat$Statistic[1], "by", time))) +
        theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
              axis.title.x = element_text(colour = "gray26", face = "bold"),
              axis.title.y = element_text(colour = "gray26", face = "bold"),
              legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
              legend.text = element_text(colour="gray26", face = "bold"))
    } else {
      den+scale_color_manual(values=pptPallete)  +
        labs(y = "Percent", x = "Precipitation (mm)") +
        ggtitle(tools::toTitleCase(paste("Density Plot of", variable, plotDat$Statistic[1], "by", time))) +
        theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
              axis.title.x = element_text(colour = "gray26", face = "bold"),
              axis.title.y = element_text(colour = "gray26", face = "bold"),
              legend.title = element_text(hjust = 0.5, colour="gray15", face = "bold"),
              legend.text = element_text(colour="gray26", face = "bold"))
    }

  } else (print("Please enter a valid type"))

  ggsave(filename = paste0(variable, statistic, time, type, ".png"), path = "/Users/cbandjelly/Desktop/MCO/Gridded_Data/FinalFigs/", width = 9, height = 4.5)

}
