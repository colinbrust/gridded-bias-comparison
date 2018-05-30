# This is a function that aggregates all other functions to create and save a
# data frame of the specified inputs

# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# variable - either "tmax", "tmin" or "ppt"
# ptFile - a pointfile across Montana

saveFile <- function(time, stat, variable, ptFile) {


  source("./R/aggregateDFs.R")
  source("./R/saveDF.R")
  library(magrittr)

  aggregateDFs(time, stat, variable, ptFile) %>%
  saveDF()

}
