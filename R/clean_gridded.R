library(magrittr)


clean_extraction <- function(dat) {
  
  dat %>% 
    tidyr::pivot_longer(-station) %>%
    tidyr::separate(name, c("date", "product", "element"), "_") %>% 
    dplyr::mutate(date = lubridate::as_date(date, format = "X%Y%m%d"))
  
}


clean_gridded_raw <- function(dat) {
  
  dat %>%
    dplyr::mutate(
      value = dplyr::case_when(
        product == 'gridmet' & (element %in% c("tmin", "tmax")) ~ value - 273.15,
        TRUE ~ value
      )
    ) %>% 
    dplyr::rename(gridded_value = value) 
  
}

clean_all <- function(
    dirname = "./data-raw/extractions", 
    out_dir = "./data-raw",
    nclimgrid = "./data-raw/raw_nclimgrid.csv"
) {
  
  nclimgrid <- readr::read_csv(nclimgrid)

  
  list.files(dirname, full.names = T) %>% 
    purrr::map(readr::read_csv, show_col_types = FALSE) %>% 
    purrr::map(clean_extraction) %>%
    dplyr::bind_rows() %>% 
    clean_gridded_raw() %>% 
    dplyr::bind_rows(nclimgrid) %>% 
    readr::write_csv(file.path(out_dir, "clean_gridded.csv"))
  
}
