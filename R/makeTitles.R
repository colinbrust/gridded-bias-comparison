# Function that makes a title for a plot based on input parameters.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# toSubset - the list of logical statements passed into the plot function

makeTitles <- function(variable, time, stat, toSubset, deviation = FALSE) {

  if (deviation == FALSE) {
    if(time == "Monthly") {

      plotTitle <- tools::toTitleCase(paste("Plot of", variable, stat, "by Month"))

    } else if (time == "Seasonal") {

      plotTitle <- tools::toTitleCase(paste("Plot of", variable, stat, "by Season"))

    } else if (time == "Annual") {

      plotTitle <- tools::toTitleCase(paste("Plot of the 30-year", variable, stat))
    }

    if(!is.null(toSubset)) {

      subtitle <- paste("Subset by:", paste(toSubset, collapse = ", "))

      return(c(plotTitle, subtitle))

    } else {

      subtitle <- ""
      return(c(plotTitle, subtitle))
    }

  } else {

    plotTitle <- tools::toTitleCase(paste("Plot of", time, "deviation from dataset average for",
                                            variable, stat))

    if(!is.null(toSubset)) {

      subtitle <- paste("Subset by:", paste(toSubset, collapse = ", "))

      return(c(plotTitle, subtitle))

    } else {

      subtitle <- ""
      return(c(plotTitle, subtitle))
    }

  }
}
