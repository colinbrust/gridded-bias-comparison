# function that creates boxplots based on the parameters put into the function.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# ... - a list of logical statements that can be used to refine the boxplots
      # (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

make_boxplots <- function(variable, time, stat, ...) {

  #library(feather)
  library(magrittr)
  library(ggplot2)
  source("./R/factor_data.R")
  source("./R/titles_box_den.R")
  source("./R/viz_box.R")
  source("./R/save_plots.R")

  plotTitle <- titles_box_den(variable, time, stat, c(...))

  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    factor_data(time)

  ggplot(dat, aes(x = Index, y = Value, fill = Dataset)) +
    geom_boxplot(color = "gray11") +
    viz_box(variable, time, plotTitle)

  save_plots(variable, time, stat, FALSE, "box", ...)
}
