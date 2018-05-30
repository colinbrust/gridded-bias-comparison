source("./R/aggregateFunctions.R")

times <- c("Annual", "Seasonal", "Monthly")
variables <- c("tmax", "tmin", "ppt")
stats <- c("Normal", "SD")


ptFile <-
  "./analysis/data/raw_data/shapefiles/ptsAttributed.shp" %>%
  sf::read_sf()

for(i in 1:length(times)) {

  for(j in 1:length(variables)) {

    for(k in 1:length(stats)) {

      saveFile(times[i], stats[k], variables[j], ptFile)
    }
  }
}
