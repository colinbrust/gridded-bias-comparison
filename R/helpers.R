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

