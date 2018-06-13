# This function generates a title for the maps that are created. All of its
# inputs are the same as the inputs for the "make_map" function.

titles_elev <-function(variable, time, stat, timeFilter, CD, dev, toSubset) {

  if (variable == "tmax" || variable == "tmin")
    var2 = "Temperature"
  else
    var2 = "Precipitation"

  if (dev)
    dev = "Ensemble Anomaly"
  else
    dev = ""

  if (time == "Monthly") {
    plotTitle <- tools::toTitleCase(paste(
                  month.name[as.numeric(timeFilter)],
                  variable,
                  stat,
                  "by elevation in Montana's",
                  CD,
                  "Climate Division\n",
                  dev))

  } else if (time == "Seasonal") {
    seasons <- c("Winter", "Spring", "Summer", "Autumn")
    plotTitle <- tools::toTitleCase(paste(
                  seasons[as.numeric(timeFilter)],
                  variable,
                  stat,
                  "by elevation in Montana's",
                  CD,
                  "Climate Division\n",
                  dev))

  } else if (time == "Annual") {
    plotTitle <- tools::toTitleCase(paste(
                  "Annual",
                  variable,
                  stat,
                  "by elevation in Montana's",
                  CD,
                  "Climate Division\n",
                  dev))
  }

  if(length(toSubset) == 0) {

    return(c(plotTitle, ""))
  } else {
    return(c(plotTitle,
             paste("Subset by:", paste(toSubset, collapse = ", "))))
  }

}
