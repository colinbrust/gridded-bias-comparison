library(magrittr)

join_datasets <- function(
    gridded_f = "./data-raw/clean_gridded.csv",
    mesonet_f = "./data-raw/daily_mesonet.csv"
) {
  
  mesonet_data <- readr::read_csv(gridded_f, show_col_types = FALSE)
  gridded_data <- readr::read_csv(mesonet_f, show_col_types = FALSE) %>% 
    dplyr::select(-tmean) %>% 
    tidyr::pivot_longer(
      c("tmax", "tmin", "ppt"), 
      names_to = "element", 
      values_to = "mesonet_value"
    )
  
  dplyr::left_join(mesonet_data, gridded_data, by = c("station", "date", "element")) %>% 
    dplyr::filter(!is.na(mesonet_value), !is.na(gridded_value))
}

rmse <- function(x, x_hat)  sqrt(mean((x_hat - x) ^ 2))
r2 <- function (x, x_hat) cor(x, x_hat) ^ 2
mse <- function(x, x_hat)  mean((x_hat - x) ^ 2)
bias <- function(x, x_hat) mean(x_hat - x)




summarize_record <- function(joined, period) {
  
  joined %>% 
    dplyr::group_by(product, element) %>%
    dplyr::summarise(
      rmse = rmse(mesonet_value, gridded_value),
      r2 = r2(mesonet_value, gridded_value),
      mse = mse(mesonet_value, gridded_value),
      bias = bias(mesonet_value, gridded_value)
    )
    
}