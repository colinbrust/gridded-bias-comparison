# Function that will create boxplots to show how much each dataset varies from
# the "ensemble" mean.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

make_dev_boxplots <- function(variable, time, stat, ...) {

  #library(feather)
  library(magrittr)
  library(ggplot2)
  source("./R/viz_den_box.R")
  source("./R/factor_data.R")
  source("./R/titles_box_den.R")
  source("./R/save_plots.R")

  plotTitle <- titles_box_den(variable, time, stat, c(...), deviation = T)

  "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    dplyr::mutate(EnsDiff = EnsDiff*-1) %>%
    factor_data(time) %>%
    ggplot(aes(x = Index, y = EnsDiff, fill = Dataset)) +
      geom_boxplot(color = "gray11") +
      viz_den_box(variable, time, plotTitle, "box")

  save_plots(variable, time, stat, TRUE, "box", ...)
}
