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
