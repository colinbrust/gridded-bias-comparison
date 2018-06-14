# Variable is either "Temperature" or "Precipitation"

make_elev_agg_plots <- function(variable, stat, CD) {

  library(mcor)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  if(variable == "Temperature") {
    tmpEval = "tmax"
    ylabel = "Median Temperature(C)"
  } else {
    tmpEval = "ppt"
    ylabel = "Median Precipitation(mm)"
  }

  save_name <- paste("./analysis/figures/elev_map_Annual", variable,
                     stat, paste0(CD, ".png"), sep = "_")

  plot_title <- paste("Annual",
                      variable,
                      stat,
                      "by Elevation\n in Montana's",
                      CD,
                      "Climate Division")

  dat <- "./analysis/data/derived_data/Extracts/" %>%
    paste0("Annual") %>%
    paste(tmpEval, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes", ClimateDivision == CD,
                  Dataset != "Ensemble")

  if(variable == "Temperature") {

    dat2 <- "./analysis/data/derived_data/Extracts/" %>%
      paste0("Annual") %>%
      paste("tmin", paste0(stat, ".feather"), sep = "_") %>%
      feather::read_feather() %>%
      dplyr::filter(Montana == "yes", ClimateDivision == CD,
                    Dataset != "Ensemble") %>%
      dplyr::select(Value, PointID, Dataset) %>%
      dplyr::rename("tmin" = Value) %>%
      dplyr::full_join(dat, by = c("PointID", "Dataset")) %>%
      dplyr::rename("tmax" = Value) %>%
      dplyr::mutate("tmean" = (tmin + tmax)/2) %>%
      dplyr::mutate(ElevBin = dplyr::ntile(Elevation, 100)) %>%
      dplyr::group_by(ElevBin, Dataset) %>%
      dplyr::summarise(tmin_median = median(tmin, na.rm = T),
                       tmean_median = median(tmean, na.rm = T),
                       tmax_median = median(tmax, na.rm = T),
                       Elevation = max(Elevation)) %>%
      tidyr::gather(key = var_type, value = median,
                    tmin_median, tmean_median, tmax_median)
  } else {

    dat2 <- dat %>%
      dplyr::mutate(ElevBin = dplyr::ntile(Elevation, 100)) %>%
      dplyr::group_by(ElevBin, Dataset) %>%
      dplyr::summarise(ppt_median = median(Value, na.rm = T),
                       Elevation = max(Elevation)) %>%
      tidyr::gather(key = var_type, value = median, ppt_median)

  }

  if(variable == "Temperature") {
    plot_colors <- c("#ff0101", "#8fabbe", "#ffaa01")
    names(plot_colors) <- c("tmax_median", "tmean_median", "tmin_median")
    color_names <- c("tmax", "tmean", "tmin")
  } else {
    plot_colors = "#4f63dd"
    names(plot_colors) = "ppt_median"
    color_names <- "ppt"
  }

  dat2$Dataset <- factor(dat2$Dataset)
  dat2$var_type <- factor(dat2$var_type)

  plots <- ggplot(data = dat2, aes(x = Elevation, y = median, colour = var_type)) +
    geom_line(size = 1) +
    scale_color_manual(name = "Variable", values = plot_colors,
                       labels = color_names) +
    ylab(ylabel) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
          axis.title.x =  element_text(colour = "gray26", face = "bold"),
          axis.title.y =  element_text(colour = "gray26", face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(colour = "gray26", face = "bold"),
          legend.text = element_text(colour = "gray26", face = "bold")) +
    facet_wrap(~Dataset, ncol = 1)

  CDs <- "./analysis/data/raw_data/shapefiles/CDs.shp" %>%
    sf::read_sf() %>%
    dplyr::mutate(highlight = dplyr::if_else(NAME == CD,
                                             TRUE, NA))

  maps <- ggplot() +
    geom_sf(data = mt_state_simple, fill = NA, color = "gray40", size = 1) +
    geom_sf(data = CDs, color = "gray20", size = 1, aes(fill = highlight)) +
    labs(title = plot_title) +
    viz_elev_agg()

  cowplot::plot_grid(maps,
                    plots,
                    ncol = 1,
                    align = "v",
                    axis = 'l',
                    rel_heights = c(1,4))

  ggplot2::ggsave(filename = save_name, width = 9, height = 12, units = "in",
                  device = "png", dpi = "print")
}

viz_elev_agg <- function(base_size = 6.5,
                          base_family = "") {
  ggplot2::theme_bw(base_size = base_size,
                    base_family = base_family) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = 'transparent'),
      panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
      legend.position = "none",
      plot.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "npc"),
      plot.title = element_text(family = "sans", size = 13, hjust = 0.5,
                                colour = "gray15", face = "bold"),
      plot.subtitle = element_text(family = "sans", size = 10, hjust = 0.5,
                                   colour = "gray20", face = "bold"),
      strip.text = element_text(family = "sans", size = 8, face = "bold",
                                hjust = 0.5, vjust = 1))
}
