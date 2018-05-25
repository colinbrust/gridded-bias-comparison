# This is a function that calculates the difference of each dataset relative to the
# ensemble average across datasets

# dat - the dataframe that difference from ensmeble average will be calculated for.

ensembleDiff <- function(dat) {

  fill <-
    dat %>%
    dplyr::filter(Dataset == "Ensemble") %>%
    dplyr::select(Value) %>%
    rep(nrow(dat)/nrow(dplyr::filter(dat, Dataset == "Ensemble"))) %>%
    unlist() %>%
    unname()

  return(tibble::add_column(dat, "EnsVal" = fill))

}
