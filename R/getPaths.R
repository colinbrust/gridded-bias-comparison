
# time is either "monthly", "seasonal" or "annual"
# statistic is either "Normal" or "SD"
# variable is either "tmin", "tmax", or "ppt"

# This function returns a list of raster images that correspond to the
# arguments entered into the function
GetImages <- function(time, statistic, variable) {

  dataNames <- c("Chirps", "PRISM", "Gridmet", "Daymet", "TopoWx")

  dataList <- list()

  paths <-
    "./analysis/data/derived_data" %>%
    list.files(full.names = T, recursive = T) %>%
    grep(statistic, ., value = TRUE) %>%
    grep(variable, ., value = TRUE) %>%
    grep(time, ., value = TRUE)

  for(i in 1:length(dataNames)) {

    fileVec <- grep(dataNames[i], paths, value = TRUE)

    if (length(fileVec) == 0) {

    } else {

      dataList[[i]] <- fileVec
    }

  }

  return(dataList)

}
