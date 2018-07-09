# This function is to be used with images that were created in Google Earth
# engine using the "Summer2018" code. It converts a multiband image into
# individual daily images.

GEE_multi_to_daily <- function(directory = "./analysis/data/raw_data/daily_data") {

  library(lubridate)
  library(magrittr)
  library(raster)
  library(dplyr)
  library(stringr)

  multis <- list.files(directory, full.names = T, pattern = ".tif")

  new_dir <- paste0(directory, "/",
                    tools::file_path_sans_ext(basename(multis)))

  lapply(new_dir, dir.create)

  fnames <- lapply(multis, image_names)

  print(length(multis))
  print(length(new_dir))
  print(length(fnames))

  mapply(daily_images, multiband = multis, dirs = new_dir, fnames = fnames)

}

image_names <- function(multiband) {

  dates <- basename(multiband) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    tail(2) %>%
    lapply(lubridate::as_date)

  dates <- seq(dates[[1]], dates[[2]], by = 'days') %>%
    gsub("-", "", .)

  basename(multiband) %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    head(2) %>%
    paste(collapse = "_") %>%
    paste(dates, sep = "_") %>%
    head(-1)

}

daily_images <- function(multiband, dirs, fnames) {

  for(i in 1:length(fnames)) {

    img_name <- paste0(dirs, "/", fnames[i], ".tif")

    img <- raster::raster(multiband, band = i)
    img[is.na(img)] <- -9999

    raster::writeRaster(img,
                        filename = img_name,
                        format = "GTiff")

    print(img_name)

  }

}
