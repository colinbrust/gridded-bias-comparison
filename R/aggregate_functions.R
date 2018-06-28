# This is a function that aggregates all other functions to create and save a
# data frame of the specified inputs

# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# variable - either "tmax", "tmin" or "ppt"
# ptFile - a pointfile across Montana

aggregate_functions <- function(time, stat, variable, ptFile, type) {


  source("./R/aggregate_dfs.R")
  source("./R/save_df.R")
  library(magrittr)

  aggregate_dfs(time, stat, variable, ptFile) %>%
  save_df(type)

}
