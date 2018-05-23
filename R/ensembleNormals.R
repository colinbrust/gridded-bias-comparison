ensembleNormals <- function(variable, time, stat) {

  dat <- "./analysis/data/derived_data/files/" %>%
    paste0(time)%>%
    paste(variable, paste0(stat, ".feather"), sep = "_") %>%
    feather::read_feather()

  dat$Index <- as.numeric(dat$Index)
  dat$PointID <- as.numeric(dat$PointID)

  dat2 <-
    dplyr::select(dat, -Value, -Dataset) %>%
    dplyr::distinct()

  evalParams <-
    dplyr::select(dat, PointID, Index) %>%
    dplyr::distinct()

  for(i in 1:nrow(evalParams)) {


  }
}



filterDat <- function(id, index, stat) {

  SDavg <- function(SDs) {

    sqrt(sum(SDs^2)/length(SDs))

  }

  if(stat == "Normal") {

    dplyr::filter(dat, PointID == id, Index == index) %>%
    dplyr::summarise(avg = mean(Value)) %>%
    as.numeric()

  } else if (stat == "SD") {

    dplyr::filter(dat, PointID == id, Index == index) %>%
    dplyr::summarise(avg = SDavg(Value)) %>%
    as.numeric()

  }

}
