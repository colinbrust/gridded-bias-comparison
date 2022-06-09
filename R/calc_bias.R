library(magrittr)
library(ggplot2)

join_datasets <- function(
    gridded_f = "./data-raw/clean_gridded.csv",
    mesonet_f = "./data-raw/daily_mesonet.csv"
) {
  
  mesonet_data <- readr::read_csv(gridded_f, show_col_types = FALSE)
  gridded_data <- readr::read_csv(mesonet_f, show_col_types = FALSE) %>% 
    tidyr::pivot_longer(
      c("tmax", "tmin", "ppt"), 
      names_to = "element", 
      values_to = "mesonet_value"
    )
  
  dplyr::left_join(mesonet_data, gridded_data, by = c("station", "date", "element")) %>% 
    dplyr::filter(
      !is.na(mesonet_value), !is.na(gridded_value), 
      !is.infinite(mesonet_value), !is.infinite(gridded_value)
    )
}

rmse <- function(x, x_hat)  sqrt(mean((x - x_hat) ^ 2))
r2 <- function (x, x_hat) cor(x, x_hat) ^ 2
mse <- function(x, x_hat)  mean((x - x_hat) ^ 2)
bias <- function(x, x_hat) mean(x - x_hat)

calc_error <- function(joined, period='annual', station=FALSE) {
  
  joined %>% 
    dplyr::mutate(
      month = lubridate::month(date),
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall"
      ), 
      annual = 1,
      month = month.name[month],
      month = factor(month, levels = month.name)
    ) %>% 
    dplyr::filter(!(element == 'ppt' & season != "Summer")) %>%
    {
      if (station) {
        dplyr::group_by(., product, element, !!rlang::sym(period), station)
      } else {
        dplyr::group_by(., product, element, !!rlang::sym(period))
      }
    } %>%
    dplyr::summarise(
      rmse = rmse(mesonet_value, gridded_value),
      r2 = r2(mesonet_value, gridded_value),
      mse = mse(mesonet_value, gridded_value),
      bias = bias(mesonet_value, gridded_value),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      element = dplyr::recode(
        element,
        'ppt' = 'Precipitation (mm)',
        'tmax' = 'Max. Temperature (deg C)',
        'tmin' = 'Min. Temperature (deg C)'
      ), 
      product = dplyr::recode(
        product,
        'daymet' = 'Daymet',
        'gridmet' = 'gridMET',
        'prism' = 'PRISM' 
      )
    ) 
}

plot_summary <- function(joined, period, stat='rmse') {
  
  stat_lab = switch(
    stat,
    'rmse' = 'RMSE',
    'bias' = 'Mean Bias',
    'mse' = 'MSE',
    'r2' = 'R-Squared'
  )
  
  period_lab = switch(
    period, 
    'month' = 'Monthly',
    'season' = 'Seasonal',
    'annual' = 'Annual'
  )
  
  title = glue::glue(
    "{period_lab} {stat_lab} for All Mesonet Stations"
  )
  
  calc_error(joined, period, FALSE) %>% 
    dplyr::select(product, element, time=period, value=stat) %>% 
    ggplot(aes(x=time, y=value, fill=product)) + 
      geom_bar(stat='identity', position='dodge') + 
      geom_hline(aes(yintercept=0)) + 
      facet_wrap(~element, nrow=3, scales='free_y') + 
      scale_fill_manual(
        values = c(
          "Daymet" = "#1b9e77",
          "gridMET" = "#d95f02",
          "PRISM" = "#7570b3"
        )
      ) +
      labs(x='', y='', fill = '', title = title) + 
      theme_bw()
}

stations <- readr::read_csv("https://mesonet.climate.umt.edu/api/v2/stations/?type=csv", 
                show_col_types = FALSE) %>%
  dplyr::select(station, name, longitude, latitude) 

joined <- join_datasets()


plot_map <- function(joined, stations, stat='rmse', element='tmin') {
  
  e_swap = switch(
    element,
    'ppt' = 'Precipitation (mm)',
    'tmax' = 'Max. Temperature (deg C)',
    'tmin' = 'Min. Temperature (deg C)'
  )
  
  mt <- urbnmapr::get_urbn_map(sf = T) %>% 
    dplyr::filter(state_abbv == 'MT') %>% 
    sf::st_transform(4326)
  
  dat <- calc_error(joined, "annual", TRUE) %>% 
    dplyr::left_join(stations, by="station") %>% 
    dplyr::filter(
      !is.na(name),
      element == e_swap) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  ggplot() + 
    geom_sf(mapping = aes(color = r2), data = dat) + 
    geom_sf(mapping = aes(), data = mt, fill = NA) + 
    scale_color_distiller(type='div') + 
    facet_wrap(~product, nrow=3) + 
    theme_minimal()
    
}