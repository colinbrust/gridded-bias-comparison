# This function creates file names for a plot and then saves the plot out
# to your drive.

# variable - either "tmax", "tmin" or "ppt".
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# deviation - either TRUE or FALSE. Indicates whether or not the plot shows the
            # deviation from the ensemble average.
# type - the type of plot that is being made.
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

save_plots <- function(variable, time, stat, deviation, type, ...) {

  # library(ggplot2)
  library(magrittr)


  if (deviation) {
    dev <- "devT"
  } else {
    dev <- "devF"
  }

  if(!is.null(c(...))) {

    sub_name <-
      c(...) %>%
      stringr::word(1) %>%
      paste(collapse = "_")

    save_name <- paste(paste0("./analysis/data/derived_data/images/", type), time, variable,
                       stat, dev, paste0(sub_name, ".png"), sep = "_")

  } else {

    save_name <- paste(paste0("./analysis/data/derived_data/images/", type), time, variable,
                       dev, paste0(stat, ".png"), sep = "_")
  }

  if(type == "corr" || type == "map") {

    ggplot2::ggsave(filename = save_name, width = 14, height = 10, units = "in",
                    device = "png", dpi = "print")

  } else if (type == "box" || type == "den") {

    ggplot2::ggsave(filename = save_name, width = 12, height = 9, units = "in",
                    device = "png", dpi = "print")
  }

}
