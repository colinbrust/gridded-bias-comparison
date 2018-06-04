# These are the functions that are needed to generate the correlation plots.

# makes the matrix lower triangular
lower_tri<-function(cormat){

  cormat[upper.tri(cormat)] <- NA
  return(cormat)

}

# calculates a correlation matrix. This function is to be used in purrr::map
my_cor <- function(d){

  d %>%
    dplyr:::select(-Index) %>%
    cor(method = "pearson") %>%
    lower_tri()
}

# converts from matrix form into tidy form
cor_mat_tidy <- function(dat) {

  rNames <- rownames(dat)

  dat %>%
    tibble::as_tibble() %>%
    tibble::add_column("Dataset1" = rNames) %>%
    tidyr::gather(Dataset2, value, -Dataset1) %>%
    dplyr::filter(!is.na(value))
}

# adds a column with the names of each dataset.
add_index_column <- function(dat) {

  col_name <- names(dat)

  for(i in 1:length(dat)) {

    index_names <- rep(col_name[i], nrow(dat[[i]]))

    dat[[i]] <- tibble::add_column(dat[[i]],
                                   index_names = col_name[i]
    )
  }

  return(dat)
}


