# this function takes a large tibble and saves it out to a directory.

# DF is the output of aggregateDFs function.

saveDF <- function(DF) {

  # library(feather)
  # library(dplyr)

  name <- paste0("./analysis/data/derived_data/files/",
                 paste(
                 dplyr::select(DF, "Time")[[1]][1],
                 dplyr::select(DF, "Variable")[[1]][1],
                 dplyr::select(DF, "Statistic")[[1]][1], sep = "_"), ".feather")


  feather::write_feather(DF, name)


}
