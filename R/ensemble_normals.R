# Function that takes the average for each point in the Montana point file
# for every dataset, every time period, every variable, and every statistic.
# It is then used to add the "ensemble" mean/SD to a dataframe with all the
# other datasets

# dat - The dataframe to add the ensemble mean to
# time - either "Annual", "Seasonal", or "Monthly"
# stat - either "Normal", or "SD"
# variable - either "tmax", "tmin" or "ppt"



ensembleNormals <- function(dat, time, stat, variable) {

  # library(dplyr)
  # library(tibble)

  # function that calculates the "average" SD by calculating the mean variance.
  SDavg <- function(SDs) {

    SDs <- SDs[!is.na(SDs)]

    sqrt(sum(SDs^2)/length(SDs))

  }

  if(time == "Annual") {

    pre = c("01")

  } else if (time == "Seasonal") {

    pre = c("01", "02", "03", "04")

  } else if (time == "Monthly") {

    pre = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  }

  for(i in 1:length(pre)) {

    addName <- paste(pre[i], "Ensemble", time, variable, stat, sep = "_")

    if(stat == "Normal") {

      toAdd <- rowMeans(dplyr::select(dat, dplyr::contains(pre[i])))
      dat <- tibble::add_column(dat, !!addName := toAdd)

    } else if (stat == "SD") {

      fillDat <- dplyr::select(dat, dplyr::contains(pre[i]))
      toAdd <- apply(fillDat, 1, SDavg)
      dat <- tibble::add_column(dat, !!addName := toAdd)

    }



  }

  dat

}










