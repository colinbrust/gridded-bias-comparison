# dfList is a list of data frames created from extractValues.R
# this function takes a list of data frames and returns a long
# format data frame from all the list of data frames

aggregate_dfs <- function(time, stat, variable, ptFile) {

  # library(tibble)
  # library(tidyr)
  # library(dplyr)
  source("./R/get_paths.R")
  source("./R/extract_values.R")
  source("./R/ensemble_normals.R")
  source("./R/ensemble_diff.R")
  library(magrittr)

  # function that changes gridmet values from Kelvin to Celsius
  change_gm <- function(dat) {

    if (grepl("Normal", colnames(dat))== TRUE &&
       (grepl("tmax", colnames(dat)) == TRUE ||
        grepl("tmin", colnames(dat)) == TRUE)) {

      indexes <- grep("Gridmet", colnames(dat))
      dat[indexes] <- dat[indexes] - 273.15
    }

    dat
  }

    get_paths(time, stat, variable) %>%
      extract_values(ptFile) %>%
      do.call("cbind", .) %>%
      tibble::as_data_frame() %>%
      change_gm() %>%
      ensemble_normals(time, stat, variable) %>%
      tibble::add_column("PointID" = ptFile$ORIG_FID)%>%
      tibble::add_column("ClimateDivision" = ptFile$CD) %>%
      tibble::add_column("Montana" = ptFile$Montana)%>%
      tibble::add_column("Aspect" = ptFile$Aspect)%>%
      tibble::add_column("Elevation" = ptFile$Elevation)%>%
      tibble::add_column("Slope" = ptFile$Slope)%>%
      tibble::add_column("Landform" = ptFile$Landform)%>%
      tidyr::gather(key = "Names",
                    value = "Value",
                    -"PointID",
                    -"ClimateDivision",
                    -"Aspect",
                    -"Elevation",
                    -"Slope",
                    -"Landform",
                    -"Montana") %>%
      tidyr::separate(col = "Names",
                      sep = "_",
                      into = c("Index", "Dataset", "Time", "Variable", "Statistic")) %>%
      ensemble_diff() %>%
      dplyr::mutate(EnsDiff = EnsVal - Value)
}

