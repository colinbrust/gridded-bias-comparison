# This function generates a title for the maps that are created. All of its
# inputs are the same as the inputs for the "make_map" function.

titles_elev <-function(variable, time, stat, timeFilter, toSubset) {

  if (time == "Monthly") {
    plotTitle <- tools::toTitleCase(
                                paste(
                                  "Plot of Elevation vs Temperature
                                  by Climate Division for",
                                  month.name[as.numeric(timeFilter)],
                                  variable,
                                  stat))

  } else if (time == "Seasonal") {
    seasons <- c("Winter", "Spring", "Summer", "Autumn")
    plotTitle <- tools::toTitleCase(
                                paste(
                                  "Plot of Elevation vs Temperature
                                  by Climate Division for",
                                  seasons[as.numeric(timeFilter)],
                                  variable,
                                  stat))

  } else if (time == "Annual") {
    plotTitle <- tools::toTitleCase(
                                paste(
                                  "Plot of Elevation vs Temperature
                                  by Climate Division for Annual",
                                  variable,
                                  stat))
  }

  if(is.na(toSubset)) {

    return(c(plotTitle, ""))
  } else {
    return(c(plotTitle,
             paste("Subset by:", paste(..., collapse = ", "))))
  }

}
