save_plots <- function(variable, time, stat, deviation, type, ...) {

  if (deviation) {
    dev <- "devT"
  } else {
    dev <- "devF"
  }

  if(!is.null(c(...))) {

    sub_name <-
      c(...) %>%
      stringr::word(1) %>%
      paste(collapse = "_")

    save_name <- paste(paste0("./analysis/data/derived_data/images/", type), time, variable,
                       stat, dev, paste0(sub_name, ".png"), sep = "_")

  } else {

    save_name <- paste(paste0("./analysis/data/derived_data/images/", type), time, variable,
                       dev, paste0(stat, ".png"), sep = "_")
  }

  if(type == "corr") {

    ggplot2::ggsave(filename = save_name, width = 14, height = 10, units = "in",
                    device = "png", dpi = "print")

  } else if (type == "map") {



  } else if (type == "box") {

    ggplot2::ggsave(filename = save_name, width = 12, height = 9, units = "in",
                    device = "png", dpi = "print")

  } else if (type == "den") {

    ggplot2::ggsave(filename = save_name, width = 12, height = 9, units = "in",
                    device = "png", dpi = "print")

  }

}
