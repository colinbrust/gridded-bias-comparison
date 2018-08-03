dates_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    tail(2) %>%
    lubridate::as_date() %>%
    {seq(.[1], .[2], by = "days")} %>%
    head(-1)

}

dataset_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    magrittr::extract(1)

}

variable_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split("_") %>%
    unlist %>%
    magrittr::extract(2)
}

station_from_fname <- function(fname) {

  fname %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    magrittr::extract(3)

}

replace_na <- function(x){
  x[x>40000] <- NA
  return(x)
}

replace_ppt <- function(x) {
  x[x>30] <- NA
  return(x)
}

variable_from_stack <- function(stack_name) {

  stack_name %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    magrittr::extract(2)

}

dataset_from_stack <- function(stack_name) {

  stack_name %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    magrittr::extract(1)

}

stations_2018 <- function() {

  list(c("arskeogh", "bentlake", "blm1arge",
         "blm2virg", "blm3mcca", "blm5kidd",
         "churchil", "conradmt", "corvalli",
         "crowagen", "ebarllob", "ftbentcb",
         "havrenmt", "huntleys", "kalispel",
         "lubrecht", "moccasin", "moltwest",
         "raplejen", "reedpoin", "sidneymt",
         "suatnasa", "turekran"),
       c("Fort Keogh ARS N", "Benton Lake W",
         "Argenta BLM", "Virginia City BLM",
         "McCartney Mtn BLM", "Kidd BLM",
         "Churchill", "Conrad ARC",
         "Corvallis ARC", "Crow Agency E",
         "Clearwater SW", "Fort Benton E",
         "Havre ARC", "Huntley ARC",
         "Kalispell ARC", "Lubrecht Forest",
         "Moccasin ARC", "Molt W",
         "Rapleje N", "Reed Point NE",
         "Sidney ARC", "Suat",
         "Turek Ranch"))
}

stations_2017 <- function() {

  list(c("conradmt", "corvalli", "ebarllob",
         "havrenmt", "huntleys", "kalispel",
         "moccasin", "sidneymt"),
       c("Conrad ARC", "Corvallis ARC",
         "Clearwater SW", "Havre ARC",
         "Huntley ARC", "Kalispell ARC",
         "Moccasin ARC", "Sidney ARC"))
}
