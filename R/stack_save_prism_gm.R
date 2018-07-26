last_image_date <- function(dataset, data_dir = "./analysis/data/raw_data/daily_data") {

  if(dataset == "gridmet") {

    return(
      paste0(data_dir, "/", dataset) %>%
        list.files(full.names = T) %>%
        grep(pattern = "Thumbs.db", x = ., invert = TRUE, value = TRUE) %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        stringr::str_split(pattern = "_") %>%
        lapply(function(x) magrittr::extract(x, 3)) %>%
        unlist() %>%
        max())

  } else if (dataset == "prism") {

    return(
      paste0(data_dir, "/", dataset) %>%
        list.files(full.names = T, pattern = ".bil") %>%
        basename() %>%
        stringr::str_split(pattern = "_") %>%
        lapply(function(x) magrittr::extract(x, 5)) %>%
        unlist() %>%
        max())
  }
}

stack_prism <- function(variable, data_dir = "./analysis/data/raw_data/daily_data") {

  date_from_fname_prism <- function(fname) {

    fname %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      stringr::str_split("_") %>%
      unlist %>%
      magrittr::extract(5) %>%
      lubridate::as_date() %>%
      gsub("-", "", .)

  }

  library(prism)

  options(prism.path = paste0(data_dir, "/prism"))

  out_name <- paste0(data_dir, "/",
                    "prism", "_",
                    variable, "_",
                    "20170101", "_",
                    last_image_date("prism"), ".tif")

  date_range <- ls_prism_data(absPath = T) %>%
    magrittr::extract2(2) %>%
    grep(pattern = variable, x = ., value = T) %>%
    lapply(date_from_fname_prism) %>%
    unlist() %>%
    as.Date(format = "%Y%m%d")

  dat <- ls_prism_data(absPath = T) %>%
    dplyr::filter(grepl(!!variable, abs_path)) %>%
    magrittr::extract2(2)

  dat[order(date_range)] %>%
    lapply(raster::raster) %>%
    raster::stack(quick = T) %>%
    raster::crop(raster::extent(-117, -101, 43, 50))
}

stack_gridmet <- function(variable, data_dir = "./analysis/data/raw_data/daily_data") {

  files_use <- list.files(paste0(data_dir, "/gridmet"),
                 full.names = T,
                 pattern = variable)

  file_dates <- files_use %>%
    lapply(function(flist) {flist %>%
                             basename %>%
                             tools::file_path_sans_ext() %>%
                             stringr::str_split(pattern = "_") %>%
                             unlist() %>%
                             tail(1)}) %>%
    unlist()

  out_name <- paste0(data_dir, "/",
                     "gridmet", "_",
                     variable, "_",
                     head(file_dates, 1), "_",
                     tail(file_dates, 1), ".tif")

  files_use %>%
    lapply(raster::raster) %>%
    raster::stack(quick = TRUE)

}

list_stacks <- function() {

  list_stack <- list(
    stack_gridmet("tmin"),
    stack_gridmet("tmax"),
    stack_gridmet("ppt"),
    stack_prism("tmin"),
    stack_prism("tmax"),
    stack_prism("ppt")
  )

  names(list_stack) <- c("gridmet_tmin", "gridmet_tmax", "gridmet_ppt",
                         "prism_tmin", "prism_tmax", "prism_ppt")

  list_stack
}

download_latest <- function() {

  source("./R/prism_gm_download.R")

  gm_date <-
    last_image_date("gridmet") %>%
    lubridate::as_date() %>%
    magrittr::add(1)

  if (gm_date == Sys.Date()) {

    print("Gridmet is up to date")

  } else {

    seq(gm_date, Sys.Date() - 1, by = "days") %>%
      lapply(get_gridmet_daily)

  }

  pris_date <-
    last_image_date("prism") %>%
    lubridate::as_date() %>%
    magrittr::add(1)

  if (pris_date == Sys.Date()) {

    print("Prism is up to date")

  } else {

    seq(pris_date, Sys.Date() - 1, by = "days") %>%
      lapply(get_prism_daily)

  }

}
