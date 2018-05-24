# dfList is a list of data frames created from extractValues.R
# this function takes a list of data frames and returns a large data frame

aggregateDFs <- function(time, stat, variable, ptFile) {

  # library(tibble)
  # library(tidyr)
  # library(dplyr)
  source("./R/getPaths.R")
  source("./R/extractValues.R")
  source("./R/ensembleNormals.R")
  library(magrittr)

  # function that changes gridmet values from Kelvin to Celsius
  changeGM <- function(dat) {

    indexes <- grep("Gridmet", colnames(dat))
    dat[indexes] <- dat[indexes] - 273.15

    dat
  }

  dat <-
    getPaths(time, stat, variable) %>%
    extractValues(ptFile) %>%
    do.call("cbind", .) %>%
    tibble::as_data_frame() %>%
    changeGM() %>%
    ensembleNormals(time, stat, variable) %>%
    tibble::add_column("PointID" = ptFile$ORIG_FID)%>%
    tibble::add_column("ClimateDivision" = ptFile$CD) %>%
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
                  -"Landform") %>%
    tidyr::separate(col = "Names",
                    sep = "_",
                    into = c("Index", "Dataset", "Time", "Variable", "Statistic"))

}
