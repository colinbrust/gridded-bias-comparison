download_prism <- function(dates = c("2017-01-01", "2017-01-03"), variable = "tmin",
                           out_dir = "./analysis/data/raw_data/daily_data/prism") {

  library(prism)
  options(prism.path = out_dir)

  get_prism_dailys(type = variable, minDate = head(dates, 1),
                   maxDate = tail(dates, 1), keepZip=F)

}
