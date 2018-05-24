
hexFile <-
  "./analysis/data/raw_data/shapefiles/hexAttributed.shp" %>%
  sf::read_sf()

makeSingleMap <- function(stuff) {

  #library(feather)
  library(ggplot2)
  source("./R/factorData.R")
  source("./R/makeTitles.R")

  plotTitle <- makeTitles(variable, time, stat, dataset)

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter_(toSubset) %>%
    factorData(time)

  appendVals <- dplyr::filter(dat, Dataset == "TopoWx")

  ptFile$Value <- appendVals$Value

  newHex <- dplyr::filter(hexFile, ORIG_FID <= 20000, ORIG_FID >=15000)

  newPt <- dplyr::filter(ptFile, ORIG_FID >=15000, ORIG_FID <=20000)

  p <- ggplot() +
    geom_sf(data = newPt, aes(fill = Value)) +
    scale_fill_gradient2(low = "#4f63dd", high = "#ff0101")

  plotly::ggplotly(p) %>%
    plotly::highlight(
      "plotly_hover",
      selected = plotly::attrs_selected(line = list(color= "black"))
    ) %>%
    widgetframe::frameWidget()

}
