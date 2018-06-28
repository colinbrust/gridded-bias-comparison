# this function takes a large tibble and saves it out to a directory.

# DF is the output of aggregateDFs function.

save_df <- function(DF, type) {

  # library(feather)
  # library(dplyr)

  name <- paste0("./analysis/data/derived_data/extracts/",
                 paste(
                 dplyr::select(DF, "Time")[[1]][1],
                 dplyr::select(DF, "Variable")[[1]][1],
                 dplyr::select(DF, "Statistic")[[1]][1], sep = "_"), ".", type)

  if(type == "feather") {
    feather::write_feather(DF, name)
  } else if (type == "Rds") {
    saveRDS(DF, name)
  }


}
