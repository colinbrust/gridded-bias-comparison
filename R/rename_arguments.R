new_dataset <- function(dataset) {

  switch(dataset,
         "prism" = "PRISM",
         "gridmet" = "gridMet",
         "daymet" = "Daymet",
         "chirps" = "CHIRPS",
         "topowx" = "TopoWx")
}

new_metric <- function(metric) {

  switch(metric,
         "r2" = "Pearson's R Correlation",
         "mae" = "Mean Absolute Error",
         "mean_bias" = "Mean Bias",
         "median_bias" = "Median Bias")
}

new_variable <- function(variable) {

  switch(variable,
         "tmax" = "Tmax",
         "tmin" = "Tmin",
         "ppt"  = "Ppt")
}
