# Function that will show how much each dataset varies from the "ensemble" mean.

# variable - either "tmax", "tmin" or "ppt"
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"

deviationBoxplots <- function(variable, time, stat) {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")
  source("./R/makeTitles.R")

  tmpPallete <- c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B")
  pptPallete <- c("#D8A499", "#5B1A18", "#D67236", "#FD6467")

  plotTitle <- makeTitles(variable, time, stat, c(...))

  toSubset = c(...)

  if(is.null(toSubset)) {
    toSubset = TRUE
  }

  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter_(toSubset) %>%
    factorData(time)

  nrow(dat)/nrow(dplyr::filter(dat, Dataset == "Ensemble"))

  dat2 <- dplyr::filter(dat, Dataset == "TopoWx")

  dat$Ensemble <- rep(1:nrow)



}
