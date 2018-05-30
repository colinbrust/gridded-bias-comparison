
# time is either "monthly", "seasonal" or "annual"
# statistic is either "Normal" or "SD"
# variable is either "tmin", "tmax", or "ppt"

# This function returns a list of raster images that correspond to the
# arguments entered into the function
getPaths <- function(time, statistic, variable) {

  library(magrittr)

  "./analysis/data/derived_data" %>%
    list.files(full.names = T, recursive = T) %>%
    grep(statistic, ., value = TRUE) %>%
    grep(variable, ., value = TRUE) %>%
    grep(time, ., value = TRUE) %>%
    grep("Thumbs.db", ., value = TRUE, invert = TRUE) %>%
    grep("feather", ., value = TRUE, invert = TRUE) %>%
    lapply(raster::raster)

}
