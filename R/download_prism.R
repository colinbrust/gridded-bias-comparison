

download_prism <- function(dates = c("2017-01-01", "2017-01-03"), variable = "tmin",
                           outdir = "./analysis/data/raw_data/daily_data/prism",
                           aoi =
                             "./analysis/data/raw_data/shapefiles/aoi.shp" %>%
                             sf::read_sf()) {

  library(prism)
  library(magrittr)

  options(prism.path = outdir)

  get_prism_dailys(type = variable, minDate = head(dates, 1),
                   maxDate = tail(dates, 1), keepZip=F)

  dat_stack <-
    ls_prism_data() %>%
    prism_stack()

  if(length(dates) == 1) {

    out_name <- paste0(outdir, "/prism_",
                       variable, "_",
                       gsub(pattern = "-", replacement = "", x = dates),
                       ".tif")

  } else {

    out_name <- paste0(outdir, "/prism_",
                       variable, "_",
                       gsub(pattern = "-", replacement = "", x = dates[1]), "_",
                       gsub(pattern = "-", replacement = "", x = dates[2]),
                       ".tif")
  }

  aoi <-
    aoi %>%
    sf::st_transform(raster::projection(dat_stack)) %>%
    raster::extent() %>%
    lapply(round)

  dat_stack %>%
    raster::crop(raster::extent(aoi[[1]], aoi[[2]], aoi[[3]], aoi[[4]])) %>%
    raster::writeRaster(filename = out_name,
                        format = "GTiff")

  list.dirs(outdir, full.names = T) %>%
    grep("PRISM", ., value = T) %>%
    unlink(recursive = T)
}






# #This function downloads gridded climate data from the PRISM http site.
#
# # start_date - Earliest date to download data from. In format "YYYY-MM-DD".
# # end_date - Day after latest date to download data from. In format "YYYY-MM-DD".
# # variable - either "tmax", "tmin", "tmean", or "ppt"
# # aoi - a shapefile that contains the boundaries to which you want to clip
#       # the image.
#
# aoi <-
#   "./analysis/data/raw_data/shapefiles/mtPoints.shp" %>%
#   sf::read_sf()
#
# download_prism <- function(start_date, end_date, variable) {
#
#   library(magrittr)
#   library(curl)
#   library(lubridate)
#
#   dates <- seq(lubridate::as_date(start_date),
#                lubridate::as_date(end_date), 1) %>%
#     gsub("-", "", .) %>%
#     head(-1)
#
#   new_dir <- paste0("./analysis/data/raw_data/daily_data/prism_", variable) %>%
#                     paste(dates[1], tail(dates,1), sep = "_")
#
#   dir.create(new_dir)
#
#   urls <- lapply(dates, function(x) "http://services.nacse.org/prism/data/public/4km" %>%
#              paste(variable, x, sep= "/"))
#
#   out_base <- paste(paste0(new_dir, "/", "prism"), variable, sep = "_")
#   out_names <- lapply(dates, function(x) out_base %>%
#                         paste(x, sep = "_"))
#
#   mapply(download.file,
#          url = urls,
#          destfile = out_names %>% paste0(".zip"),
#          method = "curl")
#
#   list.files(new_dir, full.names = T, pattern = ".zip") %>%
#     mapply(unzip, ., exdir = new_dir)
#
#   list.files(new_dir, full.names = T, pattern = ".zip") %>%
#     file.remove()
#
#   return(new_dir)
#
#
# }
#
# # this function takes the files that were downloaded using the function above,
# # crops them to a specified area, saves them as tiffs. This is done because in
# # their native form, PRISM data are in .bil format which requires a number of
# # supplementary metadata files. Geotiffs are cleaner.
#
# # new_dir - the directory name returned from the download_prism function
# # aoi     - a shapefile or extent object giving the bounds that you want
#         #   clip your image ot
# # variable - the variable name that is put into the previous function
# crop_prism <- function(new_dir, aoi, variable) {
#
#   rasters <- list.files(new_dir, full.names = T) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     unique() %>%
#     lapply(function(x) paste0(x, ".bil")) %>%
#     lapply(raster::raster)
#
#   aoi <-
#     aoi %>%
#     sf::st_transform(raster::projection(rasters[[1]])) %>%
#     raster::extent() %>%
#     lapply(round)
#
#   e <- raster::extent(aoi[[1]], aoi[[2]], aoi[[3]], aoi[[4]])
#
#   cropped <- lapply(rasters, raster::crop, y = e)
#
#   final_names <-  list.files(new_dir, full.names = T) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     lapply(tools::file_path_sans_ext) %>%
#     unique() %>%
#     lapply(basename) %>%
#     lapply(stringr::str_extract, pattern = "\\d{8}") %>%
#     lapply(function(x) paste("prism", variable, x, sep = "_") %>%
#              paste0(".tif")) %>%
#     lapply(function(x) paste0(new_dir, "/", x))
#
#   mapply(raster::writeRaster,
#          x = cropped,
#          filename = final_names,
#          format = "GTiff")
#
#   grep(list.files(new_dir, full.names = T), pattern = ".tif", invert = T, value = T) %>%
#     file.remove()
#
# }
#
# stack_images <- function(start_date, end_date, variable) {
#
#   path_name <- paste0("./analysis/data/raw_data/daily_data/prism_", variable) %>%
#     paste(
#       gsub("-", "", start_date),
#       gsub("-", "", end_date),
#     sep = "_")
#
#   raster_stack <-
#     path_name %>%
#     list.files(full.names = T) %>%
#     lapply(raster::raster) %>%
#     raster::stack(quick = T) %>%
#     raster::writeRaster(filename = paste0(path_name, ".tif"),
#                         format = "GTiff")
#
# }
#
# download_prism("2017-01-01", "2018-07-01", "ppt") %>%
#   crop_prism(aoi = aoi, variable = "ppt")
#
# download_prism("2017-01-01", "2018-07-01", "tmin") %>%
#   crop_prism(aoi = aoi, variable = "tmin")
#
# download_prism("2017-01-01", "2018-07-01", "tmax") %>%
#   crop_prism(aoi = aoi, variable = "tmax")
#
# stack_images("2017-09-01", "2017-10-01", "tmax")
