makeTitles <- function(variable, time, stat, toSubset) {

  if(time == "Monthly") {

    plotTitle <- tools::toTitleCase(paste("Plot of", variable, stat, "by Month"))

  } else if (time == "Seasonal") {

    plotTitle <- tools::toTitleCase(paste("Plot of", variable, stat, "by Season"))

  } else if (time == "Annual") {

    plotTitle <- tools::toTitleCase(paste("Plot of the 30-year", variable, stat))
  }

  if(!is.null(toSubset)) {

    subtitle <- paste("Subset by", paste(toSubset, collapse = ", "))

    return(c(plotTitle, subtitle))

  } else {

    subtitle <- ""
    return(c(plotTitle, subtitle))
  }

}
