# dfList is a list of data frames created from extractValues.R
# this function takes a list of data frames and returns a large data frame

aggregateDFs <- function(dfList) {

  do.call("cbind", dfList) %>%
    tidyr::gather(key = "Names",
                  value = "Value") %>%
    tidyr::separate(col = "Names",
                    sep = "_",
                    into = c("Index", "Dataset", "Time", "Variable", "Statistic"))

  dat <- tidyr::gather(dfinal,
         key = "Names",
         value = "Value")

  dat3 <- separate(dat2,
                   col = "Variable",
                   sep = "\\.",
                   into = c("Index", "Dataset", "Time", "Variable", "Statistic"))




}
