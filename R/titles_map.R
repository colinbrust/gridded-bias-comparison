# This function generates a title for the maps that are created. All of its
# inputs are the same as the inputs for the "make_map" function.

titles_map <-
  function(variable,
           time,
           stat,
           timeFilter,
           deviation = FALSE) {
    if (deviation == FALSE) {
      if (time == "Monthly") {
        plotTitle <-
          tools::toTitleCase(paste("Map of", month.name[as.numeric(timeFilter)],
                                   variable, stat))

      } else if (time == "Seasonal") {
        seasons <- c("Winter", "Spring", "Summer", "Autumn")
        plotTitle <-
          tools::toTitleCase(paste("Map of", seasons[as.numeric(timeFilter)],
                                   variable, stat))

      } else if (time == "Annual") {
        plotTitle <-
          tools::toTitleCase(paste("Map of the 30-year", variable, stat))
      }

    } else if (deviation == TRUE) {
      if (time == "Monthly") {
        plotTitle <-
          tools::toTitleCase(
            paste(
              "Map of",
              month.name[as.numeric(timeFilter)],
              "deviation from dataset average for",
              variable,
              stat
            )
          )

      } else if (time == "Seasonal") {
        seasons <- c("Winter", "Spring", "Summer", "Autumn")
        plotTitle <-
          tools::toTitleCase(paste(
            "Map of",
            seasons[as.numeric(timeFilter)],
            "deviation from dataset average for",
            variable,
            stat
          ))

      } else if (time == "Annual") {
        plotTitle <-
          tools::toTitleCase(paste(
            "Map of",
            time,
            "deviation from dataset average for",
            variable,
            stat
          ))
      }

    }

    return(paste0(plotTitle, "\n"))

  }
