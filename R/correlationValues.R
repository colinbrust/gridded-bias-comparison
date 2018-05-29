correlationValues <- function(variable, time, stat, ...) {

  source("./R/factorData.R")
  source("./R/makeTitles.R")

 "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter_(...) %>%
    dplyr::select(-EnsDiff, -EnsVal) %>%
    tidyr::spread(key = "Dataset", value ="Value") %>%
    dplyr::select(-PointID, -ClimateDivision, -Montana, -Aspect,-Elevation,
                  -Slope, -Landform , -Time, -Variable, -Statistic) %>%
   GGally::ggcorr(label = TRUE, label_round = 3)
   +scale_fill_manual(values=c("#5B1A18", "#D67236", "#FD6467", "#F1BB7B"))


}

newDat <- select(dat, Daymet, Ensemble, Gridmet, PRISM, TopoWx)

GGally::ggcorr(label = TRUE, label_round = 3 +
                 guides(color = FALSE, alpha = FALSE) +
                 scale_fill_manual(name="Bar",values=cols, guide="none"))

ggcorr(correlationValues(variable, time, stat),
       label_round = 3)
