library(magrittr)

clean_gridded_raw <- function(f) {
  
  readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        product == 'gridmet' & (element %in% c("tmin", "tmax")) ~ value - 273.15,
        TRUE ~ value
      )
    ) %>% 
    dplyr::rename(gridded_value = value) %>% 
    readr::write_csv(file.path(dirname(f), "clean_gridded.csv"))
}