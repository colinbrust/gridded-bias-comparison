# Function that will create density plots to show how much each dataset varies
# from the "ensemble" mean.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

make_dev_den_plots  <- function(variable, time, stat, ...) {

  #library(feather)
  library(magrittr)
  library(ggplot2)
  source("./R/viz_den.R")
  source("./R/factor_data.R")
  source("./R/titles_box_den.R")
  source("./R/save_plots.R")

  plotTitle <- titles_box_den(variable, time, stat, c(...), deviation = T)

  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    factor_data(time)

  ggplot2::ggplot(dat, aes(x = EnsDiff,  color = Dataset)) +
    geom_density(size = 1) +
    viz_den(variable, time, plotTitle) +
    facet_wrap(~Index)

  save_plots(variable, time, stat, TRUE, "den", ...)
}

