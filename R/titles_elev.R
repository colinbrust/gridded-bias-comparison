# This function generates a title for the maps that are created. All of its
# inputs are the same as the inputs for the "make_map" function.

titles_elev <-function(variable, time, stat, timeFilter, CD, dev, toSubset) {

  if (variable == "tmax" || variable == "tmin")
    var2 = "Temperature"
  else
    var2 = "Precipitation"

  if (dev)
    dev = "Ensemble Deviation"
  else
    dev = ""

  if (time == "Monthly") {
    plotTitle <- tools::toTitleCase(paste(
                    "Elevation vs",
                    var2,
                    "in Montana's\n",
                    CD,
                    "Climate Division for",
                    month.name[as.numeric(timeFilter)],
                    variable,
                    stat,
                    dev))

  } else if (time == "Seasonal") {
    seasons <- c("Winter", "Spring", "Summer", "Autumn")
    plotTitle <- tools::toTitleCase(paste(
                    "Elevation vs",
                    var2,
                    "in Montana's\n",
                    CD,
                    "Climate Division for",
                    seasons[as.numeric(timeFilter)],
                    variable,
                    stat,
                    dev))

  } else if (time == "Annual") {
    plotTitle <- tools::toTitleCase(paste(
                    "Elevation vs",
                    var2,
                    "in Montana's\n",
                    CD,
                    "Climate Division for Annual",
                    variable,
                    stat,
                    dev))
  }

  if(length(toSubset) == 0) {

    return(c(plotTitle, ""))
  } else {
    return(c(plotTitle,
             paste("Subset by:", paste(toSubset, collapse = ", "))))
  }

}
