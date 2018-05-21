library(ggplot2)
library(readr)
library(tibble)
library(tidyr)

### variable = "tmax", "tmin", or "ppt"
#time = "Month", "Season", or "Year"
#statistic = "Normal" or "SD"
#type = "box", "ecdf", or "den"
makePlot <- function(variable, time, stat, index = NULL) {

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  dataDir <- "./analysis/data/derived_data/files/"

  fileName <- paste(paste0(dataDir, time), variable, paste0(stat, ".feather"), sep = "_")

  dat <- feather::read_feather(fileName)

  dat$Dataset <- factor(dat$Dataset)

  test <- dplyr::filter(dat, ClimateDivision == "SOUTHWESTERN")

  if(time == "Monthly") {
    plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, dat$Statistic[1], "by", time))
    dat$Time <- factor(dat$Time, levels = month.name)
  } else if (time == "Seasonal") {
    plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, dat$Statistic[1], "by", time))
    dat$Index <- factor(dat$Index)#, levels = c("Winter", "Spring", "Summer", "Autumn"))
  } else if (time == "Annual") {
    plotTitle <- tools::toTitleCase(paste("Boxplot of 30-Year", variable, dat$Statistic[1]))
  }

  bp <- ggplot(dat, aes(x = Time, y = Value, fill = Dataset)) +
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




  ggsave(filename = paste0(variable, statistic, time, type, ".png"), path = "/Users/cbandjelly/Desktop/MCO/Gridded_Data/FinalFigs/", width = 9, height = 4.5)

}
