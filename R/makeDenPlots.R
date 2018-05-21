

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
