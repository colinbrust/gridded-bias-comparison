# function that creates density based on the parameters put into the function.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# ... - a list of logical statements that can be used to refine the boxplots
# (ClimateDivision == "WESTERN", Elevation > 2000, etc.)

make_den_plots <- function(variable, time, stat, ...) {

  #library(feather)
  library(magrittr)
  library(ggplot2)
  source("./R/factor_data.R")
  source("./R/titles_box_den.R")
  source("./R/save_plots.R")
  source("./R/viz_den_box.R")

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  plotTitle <- titles_box_den(variable, time, stat, c(...))

  "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    dplyr::filter_(...) %>%
    factor_data(time) %>%
    ggplot2::ggplot(aes(x = Value,  color = Dataset)) +
      geom_density(size = 1) +
      viz_den_box(variable, time, plotTitle, "den") +
      geom_hline(yintercept=0, colour="gray", size=1) +
      facet_wrap(~Index)

 save_plots(variable, time, stat, FALSE, "den", ...)

}

