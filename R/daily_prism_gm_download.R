aoi =
  "./analysis/data/raw_data/shapefiles/aoi.shp" %>%
  sf::read_sf()

daily_prism_gm_download <- function(data_dir = "./analysis/data/raw_data/daily_data") {

  library(mcor)
  library(lubridate)
  source("./R/download_prism.R")



}



dates_from_fname <- function(fname) {

  dates <-
    fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    tail(2) %>%
    lubridate::as_date()

  seq(dates[1], dates[2], by = "days") %>%
    head(-1)

}

dataset_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    head(1)

}

variable_from_fname <- function(fname) {

  var_names <- fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist

  var_names[2]
}

check_date <- function(fname) {

}

get_gridmet_daily <- function(day, data_dir = "./analysis/data/raw_data/daily_data/gridmet") {

  date_out <- gsub("-", "", day)

  mcor::mco_get_gridmet(out_dir = data_dir, dates = as.character(day))

  list.files(data_dir, full.names = T, pattern = ".Rds") %>%
    unlink()

  list.files(data_dir, full.names = T, pattern = "maximum") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_tmax_",
                                      date_out, ".nc"))

  list.files(data_dir, full.names = T, pattern = "minimum") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_tmin_",
                                      date_out, ".nc"))

  list.files(data_dir, full.names = T, pattern = "precipitation") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_ppt_",
                                      date_out, ".nc"))

}


get_prism_daily <- function(day, data_dir = "./analysis/data/raw_data/daily_data/prism", aoi) {

  download_prism(dates = day,
                 variable = "tmax",
                 outdir = data_dir,
                 aoi = aoi)

  download_prism(dates = day,
                 variable = "tmin",
                 outdir = data_dir,
                 aoi = aoi)

  download_prism(dates = day,
                 variable = "ppt",
                 outdir = data_dir,
                 aoi = aoi)

}

stack_and_save <- function(dataset, variable, data_dir = "./analysis/data/raw_data/daily_data") {

  files_use <- list.files(paste(data_dir, dataset, sep = "/"),
                 full.names = T,
                 pattern = variable)


  date_order <-
    files_use %>%
    lapply(dates_from_flist) %>%
    lapply(lubridate::as_date) %>%
    unlist() %>%
    order()


    lapply(raster::raster) %>%
    raster::stack(quick = TRUE)


}

dates_from_flist <- function(flist) {

  flist %>%
    basename %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    tail(1)
}


dates <- seq(lubridate::as_date("2017-01-01"), Sys.Date() - 1, by = "days") %>%
  lapply(get_gridmet_daily)

dates <- seq(lubridate::as_date("2017-01-01"), Sys.Date() - 1, by = "days") %>%
  lapply(get_prism_daily, aoi = aoi)

