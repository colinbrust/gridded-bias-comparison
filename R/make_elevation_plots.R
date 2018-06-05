make_elevation_plots <- function(variable, time, stat, timeFilter = "01", ...) {

  source("./R/viz_elev.R")
  source("./R/titles_elev.R")
  library(magrittr)
  library(ggplot2)

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time) %>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes", Dataset != "Ensemble",
                  Index == timeFilter) %>%
    dplyr::filter_(...) %>%
    dplyr::mutate(Elevation = round(Elevation, -2)) %>%
    dplyr::group_by(Elevation, ClimateDivision, Dataset) %>%
    dplyr::summarise(Median = median(Value, na.rm = T),
                     Q1 = as.numeric(quantile(Value)[2]),
                     Q3 = as.numeric(quantile(Value)[4]))

  plotTitle <- titles_elev(variable, time, stat, timeFilter, c(...))

  ggplot(dat, aes(x = Elevation, y = Median, color = Dataset)) +
    geom_line() +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), linetype = 2, alpha = 0) +
    viz_elev(variable, plotTitle) +
    facet_wrap(~ClimateDivision)

}
