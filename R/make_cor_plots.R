# a function that will plot the Pearson's r correlation between all datasets.

# variable - "tmax", "tmin" or "ppt". The variable to be evaluated
# time - "Monthly", "Seasonal" or "Annual". The time period to be evaluated
# stat - "Normal" or "SD". The statistic used to compare datasets.

make_cor_plots <- function(variable, time, stat, ...) {

  source("./R/viz_cor.R")
  source("./R/titles_cor.R")
  source("./R/factor_data.R")
  source("./R/save_plots.R")
  source("./R/cor_functions.R")

  library(magrittr)
  library(ggplot2)
  # library(purrr)
  # library(tibble)
  # library(dplyr)

  dat <- "./analysis/data/derived_data/extracts/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    factor_data(time) %>%
    dplyr::filter_(...) %>%
    dplyr::select(-EnsDiff, -EnsVal, -ClimateDivision,
                  -Montana, -Aspect,-Elevation, -Slope, -Landform,
                  -Time, -Variable, -Statistic) %>%
    tidyr::spread(key = "Dataset", value ="Value") %>%
    dplyr::select(-PointID) %>%
    split(.$Index) %>%
    purrr::map(my_cor) %>%
    purrr::map(cor_mat_tidy) %>%
    add_index_column() %>%
    do.call(rbind, .) %>%
    dplyr::mutate(highlight = dplyr::if_else(Dataset1 == "Ensemble" |
                                             Dataset2 == "Ensemble",
                                             TRUE, FALSE))

  if(time == "Monthly") {

    dat$index_names <- factor(dat$index_names, levels = month.name)

  } else if(time == "Seasonal") {

    dat$index_names <- factor(dat$index_names, levels = c("Winter", "Spring", "Summer", "Autumn"))

  }

  plotTitle <- titles_cor(variable, time, stat, c(...))

  if(time == "Seasonal" || time == "Monthly") {


    ggplot(data = dat, aes(Dataset1, Dataset2, fill = value, color = highlight)) +
      geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
      geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
                    family = "sans", fontface = "bold", size = 3.5) +
      labs(title = plotTitle[1], subtitle = plotTitle[2]) +
      viz_cor() +
      facet_wrap(~index_names)

  } else if (time == "Annual") {

    ggplot(data = dat, aes(Dataset1, Dataset2, fill = value, color = highlight)) +
      geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
      geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
                family = "sans", fontface = "bold", size = 3.5) +
      labs(title = plotTitle[1], subtitle = plotTitle[2]) +
      viz_cor()

  }

  save_plots(variable, time, stat, FALSE, "corr", ...)

}

