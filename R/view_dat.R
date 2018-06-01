# This function is just for bug fixing. You use it by entering the same
# arguments that you enter into a plotting function to see the data that are
# being plotted.

view_dat <- function(variable, time, stat, ...) {
  "./analysis/data/derived_data/extracts/" %>%
    paste0(time) %>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather() %>%
    dplyr::filter(Montana == "yes") %>%
    dplyr::filter_(...)

}
