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

  #save_plots(variable, time, stat, TRUE, "den", ...)
}

