make_elev_CD_plots <- function(variable, time, stat, timeFilter, CD, dev, ...) {

  source("./R/viz_elev.R")
  source("./R/titles_elev.R")

  if (dev)
    Value <-  rlang::sym("EnsDiff")
  else
    Value <-  rlang::sym("Value")

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0(time) %>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes", Index == timeFilter,
                  ClimateDivision == CD) %>%
    dplyr::filter_(...) %>%
    dplyr::mutate(EnsDiff = EnsDiff*-1) %>%
    dplyr::mutate(ElevBin = dplyr::ntile(Elevation, 100)) %>%
    dplyr::group_by(ElevBin, Dataset) %>%
    dplyr::summarise(Median = median(!!Value, na.rm = T),
                     Q1 = as.numeric(quantile(!!Value)[2]),
                     Q3 = as.numeric(quantile(!!Value)[4]),
                     Elevation = max(Elevation))

  dat$Dataset <- factor(dat$Dataset)
  plotTitle <- titles_elev(variable, time, stat, timeFilter, CD, dev, c(...))

  byDataset <- dat %>%
    dplyr::filter(Dataset != "Ensemble") %>%
    ggplot(aes(x = Elevation, y = Median, color = Dataset)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), linetype = 1, alpha = 0.2) +
    viz_by_dat() +
    facet_wrap(~Dataset)

  allData <- dat %>%
    ggplot(aes(x = Elevation, y = Median, color = Dataset)) +
    geom_line(size = 1) +
    viz_by_dat()

  legend <- cowplot::get_legend(allData)

  cPlot <- cowplot::plot_grid(byDataset + theme(legend.position = "none"),
                              allData   + theme(legend.position = "none"),
                              ncol = 1,
                              align = "v",
                              axis = 'l') +
    viz_all(variable, plotTitle)

  cowplot::plot_grid(cPlot, legend, rel_widths = c(10,1))

}

viz_by_dat <- function() {

  myColors <- c("#FFC857", "#E9724C", "#C5283D", "#481D24", "#255F85", "#F9DBBD")
  names(myColors) <- c("TopoWx", "PRISM", "Ensemble", "Daymet", "Gridmet", "Chirps")

  return(list(

    scale_colour_manual(name = "Dataset", values = myColors),

    theme_minimal(),

    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text    = element_text(family = "sans", size = 7, face = "bold",
                                      hjust = 0.5, vjust = 1),
          legend.title = element_text(colour="gray26", face = "bold", size = 10,
                                      family = "sans"),
          legend.text  = element_text(colour="gray26", size = 8,
                                      family = "sans"),
          strip.text   = element_text(family = "sans", size = 9, face = "bold",
                                      hjust = 0.5, vjust = 1))
  ))

}

viz_all <- function(variable, plotTitle) {

  if (variable == "tmax" || variable == "tmin")
    legTitle <- "Median Temperature (C)"
  else
    legTitle <- "Median Precipitation (mm)"


  return(list(

    scale_colour_manual(name = "Dataset", values = myColors),

    theme_minimal(),

    labs(title = plotTitle[1], subtitle = plotTitle[2],
         x = "Elevation" , y = legTitle),

    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title =  element_text(hjust = 0.5, colour="gray15", face = "bold",
                                       size = 10),
          legend.text =   element_text(colour="gray26", face = "bold", size = 10),
          line = element_blank())
  ))


}



