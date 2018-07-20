get_gridmet_daily <- function(day, data_dir = "./analysis/data/raw_data/daily_data") {

  library(mcor)
  library(magrittr)

  out_dir <- paste0(data_dir, "/gridmet")

  date_out <- gsub("-", "", day)

  mcor::mco_get_gridmet(out_dir = out_dir, dates = as.character(day))

  list.files(out_dir, full.names = T, pattern = ".Rds") %>%
    unlink()

  list.files(out_dir, full.names = T, pattern = "maximum") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_tmax_",
                                      date_out, ".nc"))

  list.files(out_dir, full.names = T, pattern = "minimum") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_tmin_",
                                      date_out, ".nc"))

  list.files(out_dir, full.names = T, pattern = "precipitation") %>%
    file.rename(from = ., to = paste0(dirname(.), "/",
                                      "gridmet_ppt_",
                                      date_out, ".nc"))
}

get_prism_daily <- function(day, data_dir = "./analysis/data/raw_data/daily_data") {

  library(prism)
  library(magrittr)
  source("./R/download_prism.R")

  out_dir <- paste0(data_dir, "/prism")

  download_prism(dates = day,
                 variable = "tmax",
                 out_dir = out_dir)

  download_prism(dates = day,
                 variable = "tmin",
                 out_dir = out_dir)

  download_prism(dates = day,
                 variable = "ppt",
                 out_dir = out_dir)

}
