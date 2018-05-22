
makePlot <- function(variable, time, stat, ...) {

  #library(feather)
  library(tibble)
  library(dplyr)
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

  den <-  ggplot2::ggplot(plotDat, aes(x = Value,  color = Dataset)) +
    geom_density() +
    facet_wrap(~Index)

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



}

