correlationValues <- function(variable, time, stat, ...) {

  source("./R/factorData.R")
  source("./R/makeTitles.R")

 dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    #dplyr::filter_(...) %>%
    dplyr::select(-EnsDiff, -EnsVal) %>%
    tidyr::spread(key = "Dataset", value ="Value") %>%
    dplyr::select(-PointID, -ClimateDivision, -Montana, -Aspect,-Elevation,
                  -Slope, -Landform , -Time, -Variable, -Statistic) %>%
    dplyr::group_by(Index)

    dat %>% dplyr::do(data.frame(Cor=t(cor(.[,2:5]))))


}

ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of mtcars",
           ggtheme=theme_bw)
