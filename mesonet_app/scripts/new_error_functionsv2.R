
calc_mae_bias <- function(variable) {
  "./mesonet_app/data/new_error_analysis.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(
      date <= lubridate::as_date("2018-07-25"), # for some reason mesonet data is missing on the 26th of July
      variable == !!variable
    ) %>%
    dplyr::mutate(mes_value = dplyr::if_else(
      dataset == "gridmet",
      floor_value,
      ceiling_value
    )) %>%
    dplyr::mutate(
      abs_error = abs(value - mes_value),
      bias = value - mes_value
    ) %>%
    dplyr::select(
      station, date, dataset,
      variable, abs_error, bias
    )
}

# metric = bias, abs_error
significance_test <- function(dataset1, dataset2,
                              test, variable, win = 1) {
  library(magrittr)

  dat <- calc_mae_bias(variable) %>%
    dplyr::filter(
      dataset == dataset1 | dataset == dataset2,
      date <= lowest_date(.),
      !is.na(bias)
    ) %>%
    split(.$dataset)

  dates_use <- dat[[1]]$date %>%
    unique() %>%
    lapply(function(x) {
      seq(lubridate::as_date(x),
        lubridate::as_date(x) + (win - 1),
        by = "days"
      )
    }) %>%
    head(-win)

  meds_out <- dates_use %>%
    lapply(med_calc, dat = dat) %>%
    dplyr::bind_rows()

  dplyr::left_join(dplyr::bind_rows(dat),
    meds_out,
    by = c("date", "dataset", "variable", "station")
  )
}

#### Plotting functions ####

scaleFUN <- function(x) sprintf("%.2f", x)

plot_bias <- function(dat) {
  library(ggplot2)

  dat1 <- dat %>%
    dplyr::select(-station, -abs_error, -bias) %>%
    dplyr::distinct()

  ggplot(dat1, aes(x = date, y = med_bias, color = dataset)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = bias25, ymax = bias75), linetype = 1, alpha = 0.2) +
    labs(y = "Bias (C)", x = "Date", color = "Dataset") +
    viz_mae() +
    scale_y_continuous(labels = scaleFUN)
}

bias_box <- function(dat, test) {
  dat_split <- split(dat, dat$dataset)

  test_fun <- switch(test,
    "t" = match.fun("t.test"),
    "ks" = match.fun("ks.test"),
    "mw" = match.fun("wilcox.test")
  )

  p_result <- dat %>%
    split(.$dataset) %>%
    {
      test_fun(.[[1]]$med_bias, .[[2]]$med_bias)
    } %>%
    {
      .$p.value
    }


  dat %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise(
      med_bias = median(bias),
      bias5 = quantile(bias, .05),
      bias95 = quantile(bias, .95)
    ) %>%
    ggplot(aes(x = dataset, y = med_bias, color = dataset)) +
    geom_errorbar(aes(ymin = bias5, ymax = bias95), width = 0.3) +
    geom_point(size = 2) +
    labs(
      y = "Bias (C)", x = "Dataset",
      title = "Median, 5th, and 95th Percentile of\nBias Across Timeseries",
      subtitle = paste(
        "P-Value of ", formatC(p_result, format = "e", digits = 3),
        "Between Datasets"
      )
    ) +
    viz_mae() +
    theme(legend.position = "none")
}

plot_abs <- function(dat) {
  library(ggplot2)

  ggplot(dat, aes(x = date, y = med_abs, color = dataset)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = abs25, ymax = abs75), linetype = 1, alpha = 0.2) +
    labs(y = "Absolute Error (C)", x = "Date", color = "Dataset") +
    viz_mae() +
    scale_y_continuous(labels = scaleFUN)
}

abs_box <- function(dat, test) {
  library(ggplot2)

  test_fun <- switch(test,
    "t" = match.fun("t.test"),
    "ks" = match.fun("ks.test"),
    "mw" = match.fun("wilcox.test")
  )

  p_result <- dat %>%
    split(.$dataset) %>%
    {
      test_fun(.[[1]]$med_abs, .[[2]]$med_abs)
    } %>%
    {
      .$p.value
    }


  dat %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise(
      med_abs = median(abs_error),
      abs5 = quantile(abs_error, .05),
      abs95 = quantile(abs_error, .95)
    ) %>%
    ggplot(aes(x = dataset, y = med_abs, color = dataset)) +
    geom_errorbar(aes(ymin = abs5, ymax = abs95), width = 0.3) +
    geom_point(size = 2) +
    labs(
      y = "Absolute Error (C)", x = "Dataset",
      title = "Median, 5th, and 95th Percentile of\nAbsolute Error Across Timeseries",
      subtitle = paste(
        "P-Value of ", formatC(p_result, format = "e", digits = 3),
        "Between Datasets"
      )
    ) +
    viz_mae() +
    theme(legend.position = "none")
}

daily_range_plot <- function(variable) {

  library(ggplot2)

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps")

  "./mesonet_app/data/error_analysis_range.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(variable == !!variable) %>%
    dplyr::mutate(binned = dplyr::ntile(mes_range, 50)) %>%
    dplyr::group_by(binned) %>%
    dplyr::mutate(mes_range = mean(mes_range)) %>%
    dplyr::ungroup() %>%
    ggplot(aes(x = mes_range, y = bias, group = interaction(mes_range, dataset),
               fill = dataset)) +
      geom_boxplot(outlier.size = 0.5) +
      viz_mae() +
      scale_color_manual(values = myColors) +
      labs(
        x = "Daily Temperature Range (C)", y = "Dataset Bias (C)",
        color = "Dataset",
        title = paste("Mesonet Temperature Range vs Gridded", variable, "Bias (C)")
      ) +
    ylim(-20, 20)
}

temp_range_bias <- function() {

  "./mesonet_app/data/error_analysis_range.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::select(station, date, dataset, mes_range, range) %>%
    dplyr::filter(dataset != "chirps") %>%
    dplyr:: distinct() %>%
    dplyr::mutate(range_bias = range - mes_range) %>%
    dplyr::mutate(binned = dplyr::ntile(mes_range, 50)) %>%
    dplyr::group_by(binned, dataset) %>%
    dplyr::summarise(rrange = mean(mes_range, na.rm = T),
                     brange = mean(range_bias, na.rm = T),
                     q25 = quantile(range_bias, 0.25, na.rm = T),
                     q75 = quantile(range_bias, 0.75, na.rm = T)) %>%
    ggplot(aes(x = rrange, y = brange, color = dataset)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = .2, linetype = 2) +
      viz_mae() +
      scale_color_manual(values = myColors) +
      labs(
        x = "Mesonet Temperature Range (C)", y = "Dataset Temperature Range Bias (C)",
        color = "Dataset Median\nand IQR",
        title = paste("Mesonet Temperature Range vs Gridded Temperature Range Bias")
      )

}

real_vs_bias <- function(variable) {

  myColors <- c("#E9724C", "#6d976d", "#255F85", "#F9DBBD")
  names(myColors) <- c("prism", "daymet", "gridmet", "chirps")

  "./mesonet_app/data/error_analysis_range.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(variable == !!variable,
                  date < lubridate::as_date("2018-01-01")) %>%
    dplyr::mutate(binned = dplyr::ntile(mes_value, 30)) %>%
    dplyr::group_by(dataset, binned) %>%
    dplyr::summarise(mes = mean(mes_value),
                     grd = median(bias),
                     q25 = quantile(bias, 0.25),
                     q75 = quantile(bias, 0.75)) %>%
    ggplot(aes(x = mes, y = grd, color = dataset)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = q25, ymax = q75),
                  alpha = 0.15, linetype = 2, size = .5) +
      viz_mae() +
      scale_color_manual(values = myColors) +
      labs(
        x = paste("Mesonet", variable, "(C)"),
        y = "Dataset Bias (C)",
        color = "Dataset Median\nand IQR",
        title = paste("Mesonet", variable, "vs Gridded", variable, "Bias (C)")
      )

}


plot_test <- function(dat) {
  dat %>%
    dplyr::select(date, test_result) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot() +
    geom_line(aes(x = date, y = test_result, color = "black"), size = 1) +
    labs(y = "P-Value", x = "Date") +
    ylim(0, 1) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
    viz_mae() +
    scale_color_manual(name = "P-Value", labels = "P-Value", values = "black")
}



#### Helper functions ####
lowest_date <- function(dat) {
  dat %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise(max_date = max(date)) %>%
    {
      min(.$max_date)
    }
}

error_test <- function(analysis_dates, dat, test, metric) {
  test_fun <- switch(test,
    "t" = match.fun("t.test"),
    "ks" = match.fun("ks.test"),
    "mw" = match.fun("wilcox.test")
  )

  dat %>%
    lapply(function(x) dplyr::filter(x, date %in% analysis_dates)) %>%
    lapply(function(x) dplyr::arrange(x, date, station)) %>%
    {
      test_fun(.[[1]][[metric]], .[[2]][[metric]])
    } %>%
    {
      .$p.value
    }
}

med_calc <- function(analysis_dates, dat) {
  dat %>%
    dplyr::bind_rows() %>%
    dplyr::filter(date %in% analysis_dates) %>%
    dplyr::group_by(station, dataset) %>%
    dplyr::mutate(station_bias = median(bias)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dataset) %>%
    dplyr::mutate(
      date = head(date, 1),
      med_bias = median(bias),
      bias25 = quantile(bias, .25),
      bias75 = quantile(bias, .75),
      med_abs = median(abs_error),
      abs25 = quantile(abs_error, .25),
      abs75 = quantile(abs_error, .75)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-bias, -abs_error) %>%
    dplyr::distinct()
}

viz_mae <- function() {
  return(list(
    theme_minimal(),

    theme(
      plot.title = element_text(hjust = 0.5, colour = "gray15", face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, colour = "gray20", face = "bold"),
      axis.title.x = element_text(colour = "gray26", face = "bold"),
      axis.title.y = element_text(colour = "gray26", face = "bold"),
      legend.title = element_text(
        hjust = 0.5, colour = "gray15", face = "bold",
        size = 10
      ),
      legend.text = element_text(colour = "gray26", face = "bold", size = 10)
    )
  ))
}


#### Ignore these functions ####

calc_temp_range <- function() {
  dat <- "./mesonet_app/data/error_analysis_fixed.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(
      date <= lubridate::as_date("2018-07-25"), # for some reason mesonet data is missing on the 26th of July
      variable == "tmin" | variable == "tmax",
      dataset == "mesonet_floor" | dataset == "mesonet_ceiling",
      !is.na(value)
    ) %>%
    dplyr::select(-floor_value, -ceiling_value, -floor_diff, -ceiling_diff) %>%
    tidyr::spread(key = variable, value = value) %>%
    dplyr::mutate(range = tmax - tmin) %>%
    dplyr::rename(method = dataset) %>%
    dplyr::select(-tmax, -tmin)


  "./mesonet_app/data/new_error_analysis.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::mutate(method = dplyr::if_else(
      dataset == "gridmet",
      "mesonet_floor",
      "mesonet_ceiling"
    )) %>%
    dplyr::left_join(dat, by = c("station", "date", "method"))
}

#calc_all_range() %>% readr::write_csv("./mesonet_app/data/error_analysis_range.csv")

calc_all_range <- function() {

  dat <- "./mesonet_app/data/error_analysis_fixed.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(variable == "tmax" | variable == "tmin") %>%
    dplyr::select(-floor_value, -ceiling_value, -floor_diff, -ceiling_diff) %>%
    split(.$dataset) %>%
    lapply(function(x) split_merge_dat(x)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(range = tmax - tmin) %>%
    dplyr::select(-tmax, -tmin)

  mes_vals <- dat %>%
    dplyr::filter(dataset == "mesonet_floor" | dataset == "mesonet_ceiling") %>%
    tidyr::spread(dataset, range) %>%
    dplyr::rename(ceiling_range = mesonet_ceiling,
                  floor_range = mesonet_floor)

  "./mesonet_app/data/error_analysis_fixed.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::full_join(mes_vals, by = c("station", "date")) %>%
    dplyr::mutate(
      mes_value = dplyr::if_else(
        dataset == "gridmet",
        floor_value,
        ceiling_value
      ),
      bias = dplyr::if_else(
        dataset == "gridmet",
        floor_diff,
        ceiling_diff
      ),
      mes_range = dplyr::if_else(
        dataset == "gridmet",
        floor_range,
        ceiling_range
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("floor"), -dplyr::starts_with("ceiling")) %>%
    dplyr::filter(!is.na(mes_value)) %>%
    dplyr::left_join(dat, by = c("station", "date", "dataset")) %>%
    dplyr::distinct() %>%
    dplyr::filter(dataset != "mesonet_ceiling", dataset != "mesonet_floor")
}

split_merge_dat <- function(dat) {
  dat %>%
    dplyr::mutate(unq = dplyr::row_number()) %>%
    split(.$variable) %>%
    lapply(function(x) tidyr::spread(x, variable, value)) %>%
    lapply(function(x) dplyr::select(x, -unq)) %>%
    {
      dplyr::full_join(.[[1]], .[[2]], by = c("station", "date", "dataset"))
    }
}

