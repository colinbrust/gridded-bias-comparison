
calc_mae_bias <- function(variable) {

  "./analysis/data/derived_data/Mesonet/extracts/new_error_analysis.csv" %>%
    readr::read_csv(col_types = readr::cols()) %>%
    dplyr::filter(variable == !!variable,
                  dataset != "mesonet_ceiling",
                  dataset != "mesonet_floor") %>%
    dplyr::mutate(mes_value = dplyr::if_else(dataset == "gridmet",
                                                floor_value,
                                                ceiling_value)) %>%
    dplyr::filter(!is.na(mes_value)) %>%
    dplyr::mutate(abs_error = abs(value - mes_value),
                  bias = value - mes_value) %>%
    dplyr::select(station, date, dataset,
                  variable, abs_error, bias)
}

# metric = bias, abs_error
significance_test <- function(dataset1, dataset2, metric,
                              test, variable, window = 1) {

  dat <- calc_mae_bias(variable) %>%
    dplyr::select(date, dataset, station, variable, !!metric) %>%
    dplyr::rename(error = !!metric) %>%
    dplyr::filter(dataset == dataset1 | dataset == dataset2) %>%
    dplyr::filter(date <= lowest_date(.)) %>%
    split(.$dataset)

  date_in <- dat[[1]]$date %>%
    unique() %>%
    lapply(function(x) {seq(lubridate::as_date(x),
                            lubridate::as_date(x) + (win-1),
                            by = "days")}) %>%
    head(-(win+1)) %>%
    lapply(error_test, dat = dat, test = test)
}

#### Helper functions ####
lowest_date <- function(dat) {

  dat %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise(max_date = max(date)) %>%
    {min(.$max_date)}
}

error_test <- function(analysis_dates, dat, test) {

  test_fun <- switch(test,
                     "t"=match.fun("t.test"),
                     "ks"=match.fun("ks.test"),
                     "mww"=match.fun("wilcox.test"))

  dat %>%
    lapply(function(x) dplyr::filter(x, date %in% analysis_dates)) %>%
    lapply(function(x) dplyr::arrange(x, date)) %>%
    {test_fun(.[[1]]$error, .[[2]]$error)} %>%
    {.$p.value}

}
