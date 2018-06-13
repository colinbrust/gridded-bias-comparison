# This function creates file names for a plot and then saves the plot out
# to your drive.

# timeFilter - depends on month
# variable - either "tmax", "tmin" or "ppt".
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# deviation - either TRUE or FALSE. Indicates whether or not the plot shows the
# deviation from the ensemble average.
# type - the type of plot that is being made.
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

save_maps <- function(timeFilter, variable, time, stat, deviation) {

  # library(ggplot2)

  if (deviation) {
    dev <- "devT"
  } else {
    dev <- "devF"
  }

  save_name <- paste(paste0("./analysis/figures/", "map"), timeFilter,
                     time, variable, dev, paste0(stat, ".png"), sep = "_")

  ggplot2::ggsave(filename = save_name, width = 14, height = 10, units = "in",
                  device = "png", dpi = "print")

}
