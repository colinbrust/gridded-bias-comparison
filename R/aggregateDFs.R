# dfList is a list of data frames created from extractValues.R
# this function takes a list of data frames and returns a large data frame

aggregateDFs <- function(dfList, ptFile) {

  # library(tibble)
  # library(tidyr)
  # library(dplyr)

  dat <-  do.call("cbind", dfList) %>%
          tibble::as_data_frame() %>%
          tibble::add_column(ptFile$CD) %>%
          tibble::add_column(ptFile$ORIG_FID)%>%
          dplyr::rename("ClimateDivision" = "ptFile$CD")%>%
          dplyr::rename("PointID" = "ptFile$ORIG_FID") %>%
          tidyr::gather(key = "Names",
                        value = "Value",
                        -"ClimateDivision",
                        -"PointID") %>%
          tidyr::separate(col = "Names",
                          sep = "_",
                          into = c("Index", "Dataset", "Time", "Variable", "Statistic"))

  if(dat$Variable[1] == "tmin" || dat$Variable[1] == "tmax") {

    dat$Value[dat$Dataset == "Gridmet"] = dat$Value[dat$Dataset == "Gridmet"] - 273.15
  }

  return(dat)
}
