map_titles <- function(variable, time, stat, timeFilter, toSubset, deviation = FALSE) {

  if (deviation == FALSE) {

    if(time == "Monthly") {

      plotTitle <- tools::toTitleCase(paste("Map of", month.name[as.numeric(timeFilter)],
                                            variable, stat))

    } else if (time == "Seasonal") {

      seasons <- c("Winter", "Spring", "Summer", "Autumn")
      plotTitle <- tools::toTitleCase(paste("Map of", seasons[as.numeric(timeFilter)],
                                            variable, stat))

    } else if (time == "Annual") {

      plotTitle <- tools::toTitleCase(paste("Map of the 30-year", variable, stat))
    }

  } else if(deviation == TRUE) {

    if(time == "Monthly") {

      plotTitle <- tools::toTitleCase(paste("Map of", month.name[as.numeric(timeFilter)],
                                            "deviation from dataset average for", variable, stat))

    } else if (time == "Seasonal") {

      seasons <- c("Winter", "Spring", "Summer", "Autumn")
      plotTitle <- tools::toTitleCase(paste("Map of", seasons[as.numeric(timeFilter)],
                                            "deviation from dataset average for", variable, stat))

    } else if (time == "Annual") {

      plotTitle <- tools::toTitleCase(paste("Map of", time, "deviation from dataset average for",
                                            variable, stat))
    }

  }

  if (variable == "tmin" || variable == "tmax") {

    legTitle <- "Temperature (°C)"
    palette <- "RdBu"

  } else if (variable == "ppt") {

    legTitle <- "Precipitation (mm)"
    palette <- "Blues"

  }

  if (!is.null(toSubset)) {

    subtitle <- paste("Subset by:", paste(toSubset, collapse = ", "), "\n")

    return(c(plotTitle, subtitle, legTitle, palette))

  } else {

    subtitle <- ""
    return(c(plotTitle, subtitle, legTitle, palette))
  }

}


