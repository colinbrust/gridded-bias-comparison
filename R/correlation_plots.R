correlation_plots <- function(variable, time, stat, ...) {

  source("./R/cor_viz.R")
  source("./R/cor_titles.R")
  source("./R/factor_data.R")

  # lib

  lower_tri<-function(cormat){

    cormat[upper.tri(cormat)] <- NA
    return(cormat)

  }

  my_cor <- function(d){

    d %>%
      dplyr:::select(-Index) %>%
      cor(method = "pearson") %>%
      lower_tri()
  }

  cor_mat_tidy <- function(dat) {

    rNames <- rownames(dat)

    dat %>%
      tibble::as_tibble() %>%
      tibble::add_column("Dataset1" = rNames) %>%
      tidyr::gather(Dataset2, value, -Dataset1) %>%
      dplyr::filter(!is.na(value))
  }

  add_index_column <- function(dat) {

    col_name <- names(dat)

    for(i in 1:length(dat)) {

      index_names <- rep(col_name[i], nrow(dat[[i]]))

      dat[[i]] <- tibble::add_column(dat[[i]],
                         index_names = col_name[i]
                                    )
    }

    return(dat)
  }

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
                                             Dataset2 == "Ensemble", TRUE, FALSE))

  if(time == "Monthly") {

    dat$index_names <- factor(dat$index_names, levels = month.name)

  } else if(time == "Seasonal") {

    dat$index_names <- factor(dat$index_names, levels = c("Winter", "Spring", "Summer", "Autumn"))

  }

  plotTitle <- cor_titles(variable, time, stat, c(...))

  if(time == "Seasonal" || time == "Monthly") {


    ggplot(data = dat, aes(Dataset1, Dataset2, fill = value, color = highlight)) +
      geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
      geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
                    family = "sans", fontface = "bold", size = 3.5) +
      labs(title = plotTitle[1], subtitle = plotTitle[2]) +
      cor_viz() +
      facet_wrap(~index_names)

  } else if (time == "Annual") {

    ggplot(data = dat, aes(Dataset1, Dataset2, fill = value, color = highlight)) +
      geom_tile(aes(width = 0.9, height = 0.9), size = 1.25) +
      geom_text(aes(Dataset1, Dataset2, label = round(value, 3)), color = "black",
                family = "sans", fontface = "bold", size = 3.5) +
      labs(title = plotTitle[1], subtitle = plotTitle[2]) +
      cor_viz()

  }

}

