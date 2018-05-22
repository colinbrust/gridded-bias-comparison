library(ggplot2)
library(readr)
library(tibble)
library(tidyr)


makePlot <- function(variable, time, stat, ...) {

  #library(feather)
  library(ggplot2)

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  toSubset = c(...)

  dat <- "./analysis/data/derived_data/files/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter_(toSubset)

  dat$Dataset <- factor(dat$Dataset)

  if(time == "Monthly") {

    plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, dat$Statistic[1], "by", time, toString(toSubset)))
    dat$Index <- factor(dat$Index)
    levels(dat$Index) <- month.name

  } else if (time == "Seasonal") {

    plotTitle <- tools::toTitleCase(paste("Boxplot of", variable, dat$Statistic[1], "by", time,  toString(toSubset)))
    dat$Index <- factor(dat$Index)
    levels(dat$Index) <-  c("Winter", "Spring", "Summer", "Autumn")

  } else if (time == "Annual") {

    plotTitle <- tools::toTitleCase(paste("Boxplot of 30-Year", variable, dat$Statistic[1]),  toString(toSubset))
  }

  bp <- ggplot(dat, aes(x = Index, y = Value, fill = Dataset)) +
    geom_boxplot(color = "gray11")

  if (variable == "tmax" || variable == "tmin") {
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


}
