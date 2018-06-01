

lower_tri<-function(cormat){

  cormat[upper.tri(cormat)] <- NA
  return(cormat)

}

my_cor <- function(d){

  d %>%
    dplyr:::select(-Index) %>%
    cor(method = "pearson") %>%
    lower_tri()
}

cor_mat_tidy <- function(dat) {

  rNames <- rownames(dat)

  dat %>%
    tibble::as_tibble() %>%
    tibble::add_column("Dataset1" = rNames) %>%
    tidyr::gather(Dataset2, value, -Dataset1) %>%
    dplyr::filter(!is.na(value))
}

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


