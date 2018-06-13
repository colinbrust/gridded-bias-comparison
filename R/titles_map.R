# This function generates a title for the maps that are created. All of its
# inputs are the same as the inputs for the "make_map" function.

titles_map <-
  function(variable, time, stat, timeFilter, deviation = FALSE) {

    if (deviation == FALSE) {

      if (time == "Monthly") {

        plotTitle <-
          tools::toTitleCase(paste(month.name[as.numeric(timeFilter)],
                                   variable, stat))

      } else if (time == "Seasonal") {

        seasons <- c("Winter", "Spring", "Summer", "Autumn")
        plotTitle <-
          tools::toTitleCase(paste(seasons[as.numeric(timeFilter)],
                                   variable, stat))

      } else if (time == "Annual") {
        plotTitle <-
          tools::toTitleCase(paste(time, variable, stat))
      }

    } else if (deviation == TRUE) {

      if (time == "Monthly") {
        plotTitle <-
          tools::toTitleCase(
            paste(
              month.name[as.numeric(timeFilter)],
              variable,
              stat,
              "Anomaly from Ensemble Average"
            )
          )
      } else if (time == "Seasonal") {

        seasons <- c("Winter", "Spring", "Summer", "Autumn")
        plotTitle <-
          tools::toTitleCase(paste(
            seasons[as.numeric(timeFilter)],
            variable,
            stat,
            "Anomaly from Ensemble Average"
          ))

      } else if (time == "Annual") {

        plotTitle <-
          tools::toTitleCase(paste(
            time,
            variable,
            stat,
            "Anomaly from Ensemble Average"
          ))
      }

    }

    return(paste0(plotTitle, "\n"))

  }
