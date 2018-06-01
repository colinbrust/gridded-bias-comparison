# function that factors certain columns of datasets to prepare them to be
# plotted

# dat - dataframe to be plotted
# time - either "Annual", "Seasonal", or "Monthly"

factor_data <- function(dat, time) {

  dat$Dataset <- factor(dat$Dataset)

  if(time == "Monthly") {

    dat$Index <- factor(dat$Index)
    levels(dat$Index) <- month.name
    return(dat)

  } else if (time == "Seasonal") {

    dat$Index <- factor(dat$Index)
    levels(dat$Index) <-  c("Winter", "Spring", "Summer", "Autumn")
    return(dat)

  } else if (time == "Annual") {

    dat$Index <- factor(dat$Index)
    levels(dat$Index) <- c("")
    return(dat)
  }
}
