source("./R/getPaths.R")
source("./R/extractValues.R")
source("./R/aggregateDFs.R")
source("./R/saveDF.R")
library(magrittr)


ptFile <-
  "./analysis/data/raw_data/shapefiles/ptsAttributed.shp" %>%
  sf::read_sf()

saveFile <- function(time, stat, variable) {

  getPaths(time, stat, variable) %>%
    extractValues(ptFile) %>%
    aggregateDFs(ptFile) %>%
    saveDF()

}
