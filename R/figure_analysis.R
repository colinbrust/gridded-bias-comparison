# file <- "./analysis/data/raw_data/spreadsheets/comment_sheet.csv"
#
# dat <- readr::read_csv(file) %>%
#   dplyr::filter(!is.na(comments)) %>%
#   readr::write_csv(file)

# plot_type - "elev", "corr", "box", "den", "map"

figure_analysis <- function(dataset, ptype, variable) {

  library(magrittr)

  dat <- readr::read_csv("./analysis/data/raw_data/spreadsheets/comment_sheet.csv") %>%
    dplyr::filter(dataset == !!dataset) %>%
    dplyr::mutate(fig_name = basename(figure) %>%
                           tools::file_path_sans_ext())

  dat %>%
    parse_elev() %>%
    dplyr::full_join(parse_box_den_corr(dat)) %>%
    dplyr::full_join(parse_map(dat)) %>%
    dplyr::filter(comments != "Average", plot_type == ptype, variable == !!variable) %>%
    dplyr::select(figure, comments)

}

parse_elev <- function(dat) {

  dat %>%
    dplyr::filter(grepl("elev", fig_name)) %>%
    tidyr::separate(col = fig_name,
                    sep = "_",
                    into = c("time_index", "plot_type", "time", "variable",
                             "dev", "CD", "stat"))


}

parse_box_den_corr <- function(dat) {

  dat %>%
    dplyr::filter(grepl("^.*(box|den|corr).*$", fig_name)) %>%
    tidyr::separate(col = fig_name,
                    sep = "_",
                    into = c("plot_type", "time", "variable", "dev", "stat"))
}

parse_map <- function(dat) {

  dat %>%
    dplyr::filter(grepl("map", fig_name)) %>%
    tidyr::separate(col = fig_name,
                    sep = "_",
                    into = c("plot_type", "time_index", "time", "variable",
                             "dev", "stat"))

}

