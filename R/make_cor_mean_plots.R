make_cor_mean_plots <- function(variable, stat) {

  library(magrittr)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(cowplot)
  source("./R/viz_cor_mean.R")
  source("./R/cor_functions.R")

  if (variable == "Temperature") {
    evalVar <-  c("tmax", "tmin")
  } else if(variable == "Precipitation") {
    evalVar <- "ppt"
  }

  times <-  c("Annual", "Seasonal", "Monthly")
  toEval <- expand.grid(times, evalVar, stat, stringsAsFactors = F)
  toEval <- lapply(1:nrow(toEval), function(i) toEval[i,]) %>%
    lapply(as.character) %>%
    lapply(calc_corr) %>%
    unlist(recursive = F)

  tot_mean <- apply(simplify2array(toEval), 1:2, mean) %>%
    cor_mat_tidy() %>%
    dplyr::mutate(highlight = dplyr::if_else(Dataset1 == "Ensemble" |
                                             Dataset2 == "Ensemble",
                                             TRUE, FALSE)) %>%
    ggplot(aes(Dataset1, Dataset2, fill = value, color = highlight)) +
    geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
    geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
              family = "sans", fontface = "bold", size = 3.5) +
    viz_cor_mean() +
    scale_fill_distiller(palette = "Reds",  direction = 1,
                         space = "Lab", name="Correlation Mean",
                         limit = c(0, 1)) +
    labs(title = paste("Correlation Mean and SD Across All Time Periods for",
                       variable, stat))

  tot_sd <- apply(simplify2array(toEval), 1:2, sd) %>%
    cor_mat_tidy() %>%
    dplyr::mutate(highlight = dplyr::if_else(Dataset1 == "Ensemble" |
                                               Dataset2 == "Ensemble",
                                             TRUE, FALSE)) %>%
    ggplot(aes(Dataset1, Dataset2, fill = value, color = highlight)) +
    geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
    geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
              family = "sans", fontface = "bold", size = 3.5) +
    viz_cor_mean() +
    scale_fill_distiller(palette = "Blues",  direction = 1,
                         space = "Lab", name="Correlation SD",
                         limit = c(0, 0.2))

  cowplot::plot_grid(tot_mean, tot_sd)

}

calc_corr <- function(var_list) {

  print(paste(var_list[1], var_list[2], var_list[3], sep = ", "))

  "./analysis/data/derived_data/extracts/" %>%
    paste0(var_list[1])%>%
    paste(var_list[2], paste0(var_list[3], ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::select(-EnsDiff, -EnsVal, -ClimateDivision,
                  -Montana, -Aspect,-Elevation, -Slope, -Landform,
                  -Time, -Variable, -Statistic) %>%
    tidyr::spread(key = "Dataset", value ="Value") %>%
    dplyr::select(-PointID) %>%
    split(.$Index) %>%
    purrr::map(my_cor)

}
